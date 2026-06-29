#include "dcp_directives.h"
!! p03 kernel TU — the SAME 3D stencil with two DC index orders: contiguous
!! index `i` LAST (k,j,i — cache/coalescing-friendly) vs `i` FIRST (i,j,k).
!! stdpar is order-immune (it re-collapses); OpenMP-target / CPU / acc-collapse
!! punish the wrong order heavily. p03 reports both so the gap is visible.
module kernel_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: stencil_kji, stencil_ijk

contains

   subroutine stencil_kji(nx, ny, nz, a, b)
      integer, intent(in) :: nx, ny, nz
      real(wp), intent(in)  :: a(nx, ny, nz)
      real(wp), intent(out) :: b(nx, ny, nz)
      integer :: i, j, k
      DCP_ENTER(a)
      DCP_CREATE(b)
      do concurrent(k=1:nz, j=1:ny, i=1:nx)
         b(i, j, k) = 2.0_wp*a(i, j, k) + 1.0_wp
      end do
      DCP_UPDATE_SELF(b)
      DCP_EXIT(a)
      DCP_EXIT(b)
   end subroutine stencil_kji

   subroutine stencil_ijk(nx, ny, nz, a, b)
      integer, intent(in) :: nx, ny, nz
      real(wp), intent(in)  :: a(nx, ny, nz)
      real(wp), intent(out) :: b(nx, ny, nz)
      integer :: i, j, k
      DCP_ENTER(a)
      DCP_CREATE(b)
      do concurrent(i=1:nx, j=1:ny, k=1:nz)
         b(i, j, k) = 2.0_wp*a(i, j, k) + 1.0_wp
      end do
      DCP_UPDATE_SELF(b)
      DCP_EXIT(a)
      DCP_EXIT(b)
   end subroutine stencil_ijk

end module kernel_mod
