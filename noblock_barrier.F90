module noblock_barrier
! This module implements a non-blocking barrier in OpenMP. This can be used
! to ensure that no thread enters a part of the program before all threads
! have exited some other part.

! TODO: Make the object reusable.
! TODO: Rename this module so it can contain more things.
! TODO: Rename the begin/end methods to barrier/wait or something.
! TODO: Destructor?
! TODO: Push some dependencies down into individual routines?
! TODO: Get implicit barrier out of init method?

use omp_lib, only: omp_lock_kind, omp_init_lock, omp_set_lock, &
     omp_unset_lock, omp_get_thread_num, omp_get_num_threads

implicit none
private
save

public :: soft_barrier

type soft_barrier
   ! Whether or not the barrier has been passed.
   logical, private :: complete
   ! Lock that must be acquired to set the "complete" component.
   integer(omp_lock_kind), private :: complete_lock
   ! Locks that each thread will release upon completing the barrier.
   integer(omp_lock_kind), private, allocatable :: thread_locks(:)
 contains
   procedure, pass(self) :: init => sb_init
   procedure, pass(self) :: begin => sb_begin
   procedure, pass(self) :: end => sb_end
end type soft_barrier

contains

subroutine sb_init(self)
  class(soft_barrier), intent(inout) :: self
  integer :: mynum

  mynum = omp_get_thread_num()
  !$omp single
  self%complete = .false.
  call omp_init_lock(self%complete_lock)
  allocate(self%thread_locks(omp_get_num_threads()))
  !$omp end single
  call omp_init_lock(self%thread_locks(mynum))
  call omp_set_lock(self%thread_locks(mynum))

end subroutine sb_init

subroutine sb_begin(self)
  class(soft_barrier), intent(inout) :: self
  integer :: mynum

  mynum = omp_get_thread_num()
  call omp_unset_lock(self%thread_locks(mynum))

end subroutine sb_begin

subroutine sb_end(self)
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
        do i = 1, size(self%thread_locks)
           call omp_set_lock(self%thread_locks(i))
        end do

        self%complete = .true.

        do i = 1, size(self%thread_locks)
           call omp_unset_lock(self%thread_locks(i))
        end do

     end if
     call omp_unset_lock(self%complete_lock)
  end if

end subroutine sb_end

end module noblock_barrier
