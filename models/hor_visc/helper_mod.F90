#include "dcp_directives.h"
!! hor_visc model — cross-module Smagorinsky viscosity helper, device-callable
!! from the stencil DC. nu = (Cs·dx)^2 · |D|, |D| = sqrt(tension^2 + shear^2).
module helper_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: smag_nu

contains

   pure function smag_nu(dudx, dudy, dvdx, dvdy, cs2dx2) result(nu)
      DCP_DECLARE_TARGET
      real(wp), intent(in) :: dudx, dudy, dvdx, dvdy, cs2dx2
      real(wp) :: nu, tension, shear
      tension = dudx - dvdy
      shear = dudy + dvdx
      nu = cs2dx2*sqrt(tension*tension + shear*shear)
   end function smag_nu

end module helper_mod
