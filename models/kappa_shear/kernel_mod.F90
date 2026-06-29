#include "dcp_directives.h"
!! kappa_shear model kernel — per-column, per-interface Picard fixed-point solve
!! for a diffusivity kappa = A/(1 + B·kappa^2), iterated to convergence with a
!! data-dependent (divergent) iteration count, into fixed-size `local()` stacks.
!! Archetype: iterative per-column solve (mirrors Jackson-Hallberg-Legg
!! kappa-shear's coupled column iteration / branch + occupancy stress).
module kernel_mod
   use dcp_kinds, only: wp, MAX_NZ
   implicit none
   private
   public :: kappa_shear_columns
   integer, parameter :: MAXIT = 200
   real(wp), parameter :: ITOL = 1.0e-12_wp

contains

   subroutine kappa_shear_columns(nx, ny, nz, shear, n2, kappa)
      integer, intent(in) :: nx, ny, nz
      real(wp), intent(in)  :: shear(nx, ny, nz), n2(nx, ny, nz)
      real(wp), intent(out) :: kappa(nx, ny, nz)
      integer :: i, j, k, it
      real(wp) :: kp(MAX_NZ), a, b, knew, kold
      DCP_ENTER(shear)
      DCP_ENTER(n2)
      DCP_CREATE(kappa)
      do concurrent(j=1:ny, i=1:nx) local(k, it, kp, a, b, knew, kold)
         do k = 1, nz
            a = 0.1_wp*abs(shear(i, j, k))
            b = 10.0_wp*abs(n2(i, j, k)) + 1.0_wp
            kold = a
            knew = a
            do it = 1, MAXIT
               knew = a/(1.0_wp + b*kold*kold)
               if (abs(knew - kold) < ITOL) exit
               kold = knew
            end do
            kp(k) = knew
         end do
         do k = 1, nz
            kappa(i, j, k) = kp(k)
         end do
      end do
      DCP_UPDATE_SELF(kappa)
      DCP_EXIT(shear)
      DCP_EXIT(n2)
      DCP_EXIT(kappa)
   end subroutine kappa_shear_columns

end module kernel_mod
