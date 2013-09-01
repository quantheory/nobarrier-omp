program test_counter

! Test the counter used for testing.

use omp_lib, only: omp_get_num_threads, omp_get_thread_num
use utils, only: omp_counter, test_status

implicit none

integer :: mynum, num_threads

type(omp_counter) :: counter

integer, allocatable, dimension(:) :: count0, count1, count2, count_new

type(test_status) :: status

!$omp parallel private(mynum)

mynum = omp_get_thread_num()

!$omp single
num_threads = omp_get_num_threads()
allocate(count0(num_threads), count1(num_threads), count2(num_threads), &
     count_new(num_threads))
!$omp end single

call counter%init()
!$omp single
count0 = counter%get()
!$omp end single
call counter%touch()
!$omp barrier
!$omp single
count1 = counter%get()
!$omp end single
if (mynum == 1) call counter%touch()
!$omp barrier
!$omp single
count2 = counter%get()
!$omp end single

call counter%final()
call counter%init()
count_new = counter%get()
call counter%final()

!$omp end parallel

call status%assert(all(count0 == 0))
call status%assert(all(count1 == 1))

! Only one thread touched twice.
count1(2) = 2
call status%assert(all(count1 == count2))

call status%assert(all(count_new == 0))

call status%report()

end program test_counter
