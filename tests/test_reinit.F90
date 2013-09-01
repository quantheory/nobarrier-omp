program test_reinit

! Test to ensure that the soft barriers work after being destroyed and
! reinitialized.

use omp_lib, only: omp_get_num_threads, omp_get_thread_num
use nobarrier_omp, only: soft_barrier

implicit none

type(soft_barrier) :: foo_barrier

integer, allocatable :: foo(:), bar(:)

integer :: mynum

!$omp parallel shared(foo,bar,foo_barrier) private(mynum)

mynum = omp_get_thread_num()
call foo_barrier%init()
call foo_barrier%barrier()
call foo_barrier%final()
call foo_barrier%init()

!$omp single
allocate(foo(omp_get_num_threads()))
foo = -1
!$omp end single

if (mynum == 1) call sleep(1)

foo(mynum+1) = mynum
call foo_barrier%barrier()

!$omp single
call foo_barrier%wait()
bar = foo
!$omp end single

!$omp end parallel

if (all(bar /= -1)) then
   print *, "Test passed!"
else
   print *, "Test failed!"
end if

end program test_reinit
