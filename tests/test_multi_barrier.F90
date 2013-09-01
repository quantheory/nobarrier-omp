program test_multi_barrier

! Test soft barrier requiring multiple touches.

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
call foo_barrier%init(2)

call counter%init()

! First barrier
call foo_barrier%barrier()

if (mynum == 1) call sleep(1)

call counter%touch()

! Second barrier
call foo_barrier%barrier()

!$omp single
call foo_barrier%wait()
bar = counter%get()
!$omp end single

call counter%final()

call foo_barrier%final()

!$omp end parallel

! If the "wait" command actually waited for both barriers, everyone should
! have touched the counter.
call status%assert(all(bar == 1))

call status%report()

print *, bar

end program test_multi_barrier
