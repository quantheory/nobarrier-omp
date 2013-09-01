program test_half_barrier

! Test the half_barrier type.

use omp_lib, only: omp_get_num_threads, omp_get_thread_num
use nobarrier_omp, only: half_barrier
use utils, only: omp_counter, test_status

implicit none

type(half_barrier) :: foo_barrier

type(omp_counter) :: counter

integer, allocatable :: bar(:)

integer :: i, mynum

type(test_status) :: status

!$omp parallel private(i,mynum)

mynum = omp_get_thread_num()
call foo_barrier%init()
call foo_barrier%final()
call foo_barrier%init()

call counter%init()

do i = 1, 3

   if (mynum == 1) call sleep(1)

   call counter%touch()
   call foo_barrier%barrier()

   !$omp single
   bar = counter%get()
   call status%assert(bar(2) == (i-1))
   !$omp end single nowait

end do

call counter%final()

call foo_barrier%final()

!$omp end parallel

call status%report()

end program test_half_barrier
