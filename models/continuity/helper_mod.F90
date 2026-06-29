#include "dcp_directives.h"
!! continuity model — cross-module slope limiter (van Leer), device-callable
!! from the flux DC. Mirrors the per-face limiter in continuity-PPM.
module helper_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: vanleer

contains

   pure function vanleer(r) result(phi)
      DCP_DECLARE_TARGET
      real(wp), intent(in) :: r
      real(wp) :: phi
      phi = (r + abs(r))/(1.0_wp + abs(r))   ! van Leer flux limiter
   end function vanleer

end module helper_mod
