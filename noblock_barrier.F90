module noblock_barrier
! This module implements a non-blocking barrier in OpenMP. This can be used
! to ensure that no thread enters a part of the program before all threads
! have exited some other part.

type soft_barrier
 contains
   procedure, pass(self) :: init => sb_init
   procedure, pass(self) :: begin => sb_begin
   procedure, pass(self) :: end => sb_end
end type soft_barrier

contains

subroutine sb_init(self)
  class(soft_barrier), intent(inout) :: self
end subroutine sb_init

subroutine sb_begin(self)
  class(soft_barrier), intent(inout) :: self
  !$omp barrier
end subroutine sb_begin

subroutine sb_end(self)
  class(soft_barrier), intent(inout) :: self
end subroutine sb_end

end module noblock_barrier
