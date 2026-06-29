#include "dcp_directives.h"
!! remap model kernel — per-column conservative donor-cell (PCM) overlap remap
!! from a source vertical grid to a target grid of equal column height. Each
!! (i,j) thread owns fixed-size `local()` interface/scratch stacks and walks the
!! source/target overlap. Archetype: per-column serial work with stack arrays
!! (a conservative per-column ALE-style vertical remap). Conserves the column
!! integral Σ q·dz to round-off.
module kernel_mod
   use dcp_kinds, only: wp, MAX_NZ
   implicit none
   private
   public :: remap_columns

contains

   subroutine remap_columns(nx, ny, nz, dz_src, dz_tgt, q_src, q_tgt)
      integer, intent(in) :: nx, ny, nz
      real(wp), intent(in)  :: dz_src(nx, ny, nz), dz_tgt(nx, ny, nz), q_src(nx, ny, nz)
      real(wp), intent(out) :: q_tgt(nx, ny, nz)
      integer :: i, j, k, m
      real(wp) :: zs(MAX_NZ + 1), zt(MAX_NZ + 1)
      real(wp) :: integ, lo, hi, ov
      DCP_ENTER(dz_src)
      DCP_ENTER(dz_tgt)
      DCP_ENTER(q_src)
      DCP_CREATE(q_tgt)
      do concurrent(j=1:ny, i=1:nx) local(k, m, zs, zt, integ, lo, hi, ov)
         zs(1) = 0.0_wp
         zt(1) = 0.0_wp
         do k = 1, nz
            zs(k + 1) = zs(k) + dz_src(i, j, k)
            zt(k + 1) = zt(k) + dz_tgt(i, j, k)
         end do
         do m = 1, nz
            integ = 0.0_wp
            do k = 1, nz
               lo = max(zt(m), zs(k))
               hi = min(zt(m + 1), zs(k + 1))
               ov = hi - lo
               if (ov > 0.0_wp) integ = integ + q_src(i, j, k)*ov
            end do
            if (dz_tgt(i, j, m) > 1.0e-30_wp) then
               q_tgt(i, j, m) = integ/dz_tgt(i, j, m)
            else
               q_tgt(i, j, m) = 0.0_wp
            end if
         end do
      end do
      DCP_UPDATE_SELF(q_tgt)
      DCP_EXIT(dz_src)
      DCP_EXIT(dz_tgt)
      DCP_EXIT(q_src)
      DCP_EXIT(q_tgt)
   end subroutine remap_columns

end module kernel_mod
