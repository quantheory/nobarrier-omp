program test_barrier

! Test to ensure that a nonblocking barrier actually doesn't block!

use omp_lib, only: omp_get_num_threads, omp_get_thread_num
use noblock_barrier, only: soft_barrier

type(soft_barrier) :: foo_barrier

integer, allocatable :: foo(:), bar(:)

integer :: mynum

!$omp parallel private(mynum)

mynum = omp_get_thread_num()
call foo_barrier%init()

!$omp single
allocate(foo(omp_get_num_threads()))
foo = -1
!$omp end single

if (mynum == 1) call sleep(1)

call foo_barrier%begin()

foo(mynum+1) = mynum

!$omp single
bar = foo
!$omp end single

!$omp end parallel

if (bar(2) == -1) then
   print *, "Test passed!"
else
   print *, "Test failed!"
end if

end program test_barrier
