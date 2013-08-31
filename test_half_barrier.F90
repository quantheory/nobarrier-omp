program test_half_barrier

! Test the half_barrier type.

use omp_lib, only: omp_get_num_threads, omp_get_thread_num
use noblock_omp, only: half_barrier

implicit none

type(half_barrier) :: foo_barrier

integer, allocatable :: foo(:)

integer :: i, mynum

logical :: test_passed

!$omp parallel shared(foo,test_passed,foo_barrier) private(i,mynum)

mynum = omp_get_thread_num()
call foo_barrier%init()

!$omp single
allocate(foo(omp_get_num_threads()))
foo = 0
test_passed = .true.
!$omp end single

do i = 1, 10

   if (mynum == 1) call sleep(1)

   foo(mynum+1) = mynum*i
   call foo_barrier%barrier()

   !$omp single
   test_passed = test_passed .and. (foo(1) == mynum*(i-1))
   !$omp end single nowait

end do

!$omp end parallel

if (test_passed) then
   print *, "Test passed!"
else
   print *, "Test failed!"
end if

end program test_half_barrier
