#include "dcp_directives.h"
!! vert_visc model kernel — backward-Euler vertical diffusion with no-flux BCs,
!! solved as a per-column tridiagonal (Thomas algorithm) in fixed-size `local()`
!! stacks. Archetype: per-column direct solve (a PP81-style + backward-Euler
!! vertical-mixing tridiagonal). Conserves the column integral ΣT.
module kernel_mod
   use dcp_kinds, only: wp, MAX_NZ
   implicit none
   private
   public :: vdiff_columns

contains

   subroutine vdiff_columns(nx, ny, nz, t, alpha)
      integer, intent(in) :: nx, ny, nz
      real(wp), intent(inout) :: t(nx, ny, nz)
      real(wp), intent(in) :: alpha            !! kappa*dt/dz^2
      integer :: i, j, k
      real(wp) :: aa(MAX_NZ), bb(MAX_NZ), cc(MAX_NZ)
      real(wp) :: dd(MAX_NZ), cp(MAX_NZ), m
      DCP_ENTER(t)
      do concurrent(j=1:ny, i=1:nx) local(k, aa, bb, cc, dd, cp, m)
         do k = 1, nz
            aa(k) = -alpha
            cc(k) = -alpha
            bb(k) = 1.0_wp + 2.0_wp*alpha
            dd(k) = t(i, j, k)
         end do
         aa(1) = 0.0_wp; cc(nz) = 0.0_wp           ! no-flux ends
         bb(1) = 1.0_wp + alpha
         bb(nz) = 1.0_wp + alpha
         ! Thomas forward sweep
         cp(1) = cc(1)/bb(1)
         dd(1) = dd(1)/bb(1)
         do k = 2, nz
            m = bb(k) - aa(k)*cp(k - 1)
            cp(k) = cc(k)/m
            dd(k) = (dd(k) - aa(k)*dd(k - 1))/m
         end do
         ! back substitution
         do k = nz - 1, 1, -1
            dd(k) = dd(k) - cp(k)*dd(k + 1)
         end do
         do k = 1, nz
            t(i, j, k) = dd(k)
         end do
      end do
      DCP_UPDATE_SELF(t)
      DCP_EXIT(t)
   end subroutine vdiff_columns

end module kernel_mod
