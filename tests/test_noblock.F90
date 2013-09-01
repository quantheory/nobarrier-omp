program test_noblock

! Test to ensure that a nonblocking barrier actually doesn't block!

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

call counter%init()

if (mynum == 1) call sleep(1)

call foo_barrier%barrier()

call counter%touch()

!$omp single
bar = counter%get()
!$omp end single

call counter%final()

call foo_barrier%final()

!$omp end parallel

! Since thread 1 was asleep at the time that counter%get was called, it
! should not have touched the counter yet, so the count is 0.
call status%assert(bar(2) == 0)
call status%report()

end program test_noblock
