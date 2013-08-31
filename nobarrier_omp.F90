module nobarrier_omp
! This module implements non-blocking barriers in OpenMP. These can be used
! to ensure that no thread enters a part of the program before all threads
! have exited some other part.

use omp_lib, only: omp_lock_kind, omp_get_thread_num

implicit none
private
save

public :: soft_barrier
public :: half_barrier

type soft_barrier
   ! Whether or not the barrier has been passed.
   logical, private :: complete
   ! Lock that must be acquired to set the "complete" component.
   integer(omp_lock_kind), private :: complete_lock
   ! Locks that each thread will release upon completing the barrier.
   integer(omp_lock_kind), private, allocatable :: thread_locks(:)
 contains
   procedure, pass(self) :: init => sb_init
   procedure, pass(self) :: reset => sb_reset
   procedure, pass(self) :: barrier => sb_barrier
   procedure, pass(self) :: wait => sb_wait
end type soft_barrier

type half_barrier
   integer, private, allocatable :: idxs(:)
   type(soft_barrier), private :: sb_pool(0:3)
contains
  procedure, pass(self) :: init => hb_init
  procedure, pass(self) :: barrier => hb_barrier
end type half_barrier

contains

subroutine sb_init(self)
  use omp_lib, only: omp_init_lock, omp_get_num_threads
  class(soft_barrier), intent(inout) :: self
  integer :: mynum

  mynum = omp_get_thread_num()
  !$omp single
  call omp_init_lock(self%complete_lock)
  allocate(self%thread_locks(0:omp_get_num_threads()-1))
  !$omp end single
  call omp_init_lock(self%thread_locks(mynum))
  call self%reset()
  !$omp barrier

end subroutine sb_init

subroutine sb_reset(self)
  use omp_lib, only: omp_set_lock
  class(soft_barrier), intent(inout) :: self
  integer :: mynum

  mynum = omp_get_thread_num()
  call omp_set_lock(self%thread_locks(mynum))
  !$omp single
  self%complete = .false.
  !$omp end single nowait

end subroutine sb_reset

subroutine sb_barrier(self)
  use omp_lib, only: omp_unset_lock
  class(soft_barrier), intent(inout) :: self
  integer :: mynum

  mynum = omp_get_thread_num()
  call omp_unset_lock(self%thread_locks(mynum))

end subroutine sb_barrier

subroutine sb_wait(self)
  use omp_lib, only: omp_set_lock, omp_unset_lock
  class(soft_barrier), intent(inout) :: self
  integer :: i

  ! Do nothing if the barrier is already complete before starting, or is
  ! completed by the time we get the first lock.
  if (.not. self%complete) then
     call omp_set_lock(self%complete_lock)
     if (.not. self%complete) then

        ! In this case, we need to complete the barrier. Get all the thread
        ! locks to ensure that all threads are done reading, then set
        ! the complete flag and release everything.
        do i = 0, ubound(self%thread_locks, 1)
           call omp_set_lock(self%thread_locks(i))
        end do

        self%complete = .true.

        do i = 0, ubound(self%thread_locks, 1)
           call omp_unset_lock(self%thread_locks(i))
        end do

     end if
     call omp_unset_lock(self%complete_lock)
  end if

end subroutine sb_wait

subroutine hb_init(self)
  use omp_lib, only: omp_get_num_threads
  class(half_barrier), intent(inout) :: self
  integer :: i, mynum

  mynum = omp_get_thread_num()

  !$omp single
  allocate(self%idxs(0:omp_get_num_threads()-1))
  !$omp end single

  self%idxs(mynum) = 0

  do i = 0, 3
     call self%sb_pool(i)%init()
  end do

  call self%sb_pool(2)%barrier()
  call self%sb_pool(2)%wait()
  call self%sb_pool(3)%barrier()
  call self%sb_pool(3)%wait()
  call self%sb_pool(0)%barrier()

end subroutine hb_init

subroutine hb_barrier(self)
  class(half_barrier), intent(inout) :: self
  integer :: mynum

  mynum = omp_get_thread_num()

  call self%sb_pool(add_mod4(self%idxs(mynum),2))%reset()
  call self%sb_pool(self%idxs(mynum))%wait()
  call self%sb_pool(add_mod4(self%idxs(mynum),1))%barrier()

  self%idxs(mynum) = add_mod4(self%idxs(mynum), 1)

end subroutine hb_barrier

! Utility function to find m+n mod 4.
pure function add_mod4(m, n) result(p)
  integer, intent(in) :: m, n
  integer :: p
  p = mod(m+n,4)
end function add_mod4

end module nobarrier_omp
