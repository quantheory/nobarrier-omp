module utils

! Utility code for nobarrier_omp tests.

use omp_lib, only: omp_get_thread_num

implicit none
private
save

public :: omp_counter
public :: test_status

! Counter for each thread in an OpenMP region.
type :: omp_counter
   integer, private, allocatable :: counts(:)
 contains
   procedure, pass(self) :: init => counter_init
   procedure, pass(self) :: touch => counter_touch
   procedure, pass(self) :: get => counter_get
   procedure, pass(self) :: final => counter_final
end type omp_counter

! Status for one test.
type :: test_status
   logical :: test_passed = .true.
 contains
   procedure, pass(self) :: assert => test_assert
   procedure, pass(self) :: report => test_report
end type test_status

contains

subroutine counter_init(self)
  use omp_lib, only: omp_get_num_threads

  class(omp_counter), intent(inout) :: self

  !$omp single
  allocate(self%counts(0:omp_get_num_threads()-1))
  !$omp end single

  self%counts(omp_get_thread_num()) = 0

  !$omp barrier

end subroutine counter_init

subroutine counter_touch(self)
  class(omp_counter), intent(inout) :: self
  integer :: mynum

  mynum = omp_get_thread_num()

  self%counts(mynum) = self%counts(mynum) + 1

end subroutine counter_touch

function counter_get(self) result(count_out)
  class(omp_counter), intent(in) :: self
  integer :: count_out(size(self%counts))

  count_out = self%counts

end function counter_get

subroutine counter_final(self)

  class(omp_counter), intent(inout) :: self

  !$omp barrier
  !$omp single
  deallocate(self%counts)
  !$omp end single

end subroutine counter_final

subroutine test_assert(self, assert)
  class(test_status), intent(inout) :: self
  logical, intent(in) :: assert

  !$omp atomic
  self%test_passed = self%test_passed .and. assert

end subroutine test_assert

subroutine test_report(self)
  class(test_status), intent(in) :: self

  if (self%test_passed) then
     print *, "Test passed!"
  else
     print *, "Test failed!"
  end if

end subroutine test_report

end module utils
