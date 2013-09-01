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
   procedure, pass(self) :: final => sb_final
end type soft_barrier

type half_barrier
   integer, private, allocatable :: idxs(:)
   type(soft_barrier), private :: sb_pool(0:2)
contains
  procedure, pass(self) :: init => hb_init
  procedure, pass(self) :: barrier => hb_barrier
  procedure, pass(self) :: final => hb_final
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

subroutine sb_final(self)
  use omp_lib, only: omp_destroy_lock
  class(soft_barrier), intent(inout) :: self

  integer :: mynum

  ! Precondition: The "barrier" method must be called by each thread before
  ! it reaches here, so that no thread is holding a lock after the barrier.

  mynum = omp_get_thread_num()
  !$omp barrier

  call omp_destroy_lock(self%thread_locks(mynum))
  !$omp single
  call omp_destroy_lock(self%complete_lock)
  !$omp end single

  ! Break into two single sections because we actually want the barrier
  ! between them; all omp_destroy_lock calls should be finished before
  ! deallocating the lock array.

  !$omp single
  deallocate(self%thread_locks)
  !$omp end single

end subroutine sb_final

subroutine hb_init(self)
  use omp_lib, only: omp_get_num_threads
  class(half_barrier), intent(inout) :: self
  integer :: i, mynum

  mynum = omp_get_thread_num()

  !$omp single
  allocate(self%idxs(0:omp_get_num_threads()-1))
  !$omp end single

  self%idxs(mynum) = 0

  do i = 0, 2
     call self%sb_pool(i)%init()
  end do

  !  Lock 0 will be waiting next, so hit the barrier.
  !  Lock 1 will be hitting barrier next, so it is ready as-is.
  !  Lock 2 will be reset next, so we need to cycle it through.
  call self%sb_pool(2)%barrier()
  call self%sb_pool(2)%wait()
  call self%sb_pool(0)%barrier()

end subroutine hb_init

subroutine hb_barrier(self)
  class(half_barrier), intent(inout) :: self
  integer :: mynum

  mynum = omp_get_thread_num()

  ! In mod 3 arithmetic, here's what's happening. Assume self%idxs(mynum)
  ! is 0, for simplicity.
  ! sb_pool(0) waits here, to make sure that no thread can exit until the
  ! previous half barrier call is complete.
  ! sb_pool(1) hits the barrier afterward, to set up conditions for the
  ! next waiting call.
  ! sb_pool(2) is reset so can it can hit the barrier next; in order to
  ! guarantee that the reset is complete by the time it is used, it must
  ! reset before the barrier from sb_pool(1).
  call self%sb_pool(self%idxs(mynum))%wait()
  call self%sb_pool(add_mod3(self%idxs(mynum),2))%reset()
  call self%sb_pool(add_mod3(self%idxs(mynum),1))%barrier()

  self%idxs(mynum) = add_mod3(self%idxs(mynum), 1)

end subroutine hb_barrier

subroutine hb_final(self)
  class(half_barrier), intent(inout) :: self
  integer :: i, mynum

  mynum = omp_get_thread_num()

  ! Release locks before the soft_barrier can be destroyed.
  call self%sb_pool(add_mod3(self%idxs(mynum),1))%barrier()

  do i = 0, 2
     call self%sb_pool(i)%final()
  end do

  ! Rely on the barriers in the above final method to isolate the last use
  ! of self%idxs from this deallocation.

  !$omp single
  deallocate(self%idxs)
  !$omp end single

end subroutine hb_final

! Utility function to find m+n mod 3.
pure function add_mod3(m, n) result(p)
  integer, intent(in) :: m, n
  integer :: p
  p = mod(m+n,3)
end function add_mod3

end module nobarrier_omp
