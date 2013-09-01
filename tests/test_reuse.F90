program test_reuse

! Test to ensure that a soft barrier can be reset and reused.

use omp_lib, only: omp_get_num_threads, omp_get_thread_num
use nobarrier_omp, only: soft_barrier
use utils, only: omp_counter, test_status

implicit none

type(soft_barrier) :: foo_barrier

type(omp_counter) :: counter

integer, allocatable :: bar(:)

integer :: mynum

type(test_status) :: status

!$omp parallel private(mynum)

mynum = omp_get_thread_num()
call foo_barrier%init()
call foo_barrier%barrier()
call foo_barrier%wait()
!$omp barrier
call foo_barrier%reset()
!$omp barrier

call counter%init()

if (mynum == 1) call sleep(1)

call counter%touch()
call foo_barrier%barrier()

!$omp single
call foo_barrier%wait()
bar = counter%get()
!$omp end single

call counter%final()

call foo_barrier%final()

!$omp end parallel

call status%assert(all(bar == 1))

call status%report()

end program test_reuse
