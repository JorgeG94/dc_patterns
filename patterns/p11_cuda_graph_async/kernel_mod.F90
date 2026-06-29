#include "dcp_directives.h"
!! p11 kernel TU — many tiny back-to-back DCs (the launch-bound regime). A bare
!! DC under stdpar syncs per loop and cannot be CUDA-graph-captured; wrapping
!! each in `!$acc kernels async(1)` (NVHPC-OpenACC) queues them so the runtime
!! can capture/replay a graph, with a single trailing wait. The wrap is inert on
!! the omp/host backends (the DC just runs synchronously) — same result. Mirrors
!! the barotropic fast loop's many small per-substep kernels.
module kernel_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: many_small

contains

   subroutine many_small(n, nrep, a, b)
      integer, intent(in) :: n, nrep
      real(wp), intent(in)    :: a(n)
      real(wp), intent(inout) :: b(n)
      integer :: i, r
      DCP_ENTER(a)
      DCP_ENTER(b)
      do r = 1, nrep
         DCP_KERNELS_ASYNC
         do concurrent(i=1:n)
            b(i) = b(i) + a(i)
         end do
         DCP_KERNELS_END
      end do
      DCP_WAIT
      DCP_UPDATE_SELF(b)
      DCP_EXIT(a)
      DCP_EXIT(b)
   end subroutine many_small

end module kernel_mod
