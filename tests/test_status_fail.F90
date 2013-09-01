program test_status_pass

  ! Test that the status from the test utilities can pass.

  use utils, only: test_status

  implicit none

  type(test_status) :: status

  call status%assert(.false.)
  call status%report()

end program test_status_pass
