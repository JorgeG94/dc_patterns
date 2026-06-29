#include "dcp_directives.h"
!! p02 kernel TU — the SAME DC stencil written two ways across a TU boundary:
!! explicit-shape `a(nx,ny,nz)` (the flat-impl form production uses) vs
!! assumed-shape `a(:,:,:)`. Both are correct; on NVHPC the assumed-shape dummy
!! makes each launch walk the array descriptor (per-launch memcpys), so the
!! explicit form is markedly faster. p02 reports both so the gap is visible.
module kernel_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: apply_explicit, apply_assumed

contains

   subroutine apply_explicit(nx, ny, nz, a, b)
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
   end subroutine apply_explicit

   subroutine apply_assumed(a, b)
      real(wp), intent(in)  :: a(:, :, :)
      real(wp), intent(out) :: b(:, :, :)
      integer :: i, j, k, nx, ny, nz
      nx = size(a, 1); ny = size(a, 2); nz = size(a, 3)
      DCP_ENTER(a)
      DCP_CREATE(b)
      do concurrent(k=1:nz, j=1:ny, i=1:nx)
         b(i, j, k) = 2.0_wp*a(i, j, k) + 1.0_wp
      end do
      DCP_UPDATE_SELF(b)
      DCP_EXIT(a)
      DCP_EXIT(b)
   end subroutine apply_assumed

end module kernel_mod
