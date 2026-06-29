#include "dcp_directives.h"
!! p01 helper TU — a `pure` pointwise function in its OWN module, called from a
!! `do concurrent` in a DIFFERENT module (kernel_mod). Carrying
!! DCP_DECLARE_TARGET (`!$acc routine seq` / `!$omp declare target`) is what
!! makes it device-callable across the TU boundary — the pattern NVHPC will not
!! inline across modules. Mirrors an equation-of-state / pointwise math helper.
module helper_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: poly

contains

   pure function poly(x) result(y)
      DCP_DECLARE_TARGET
      real(wp), intent(in) :: x
      real(wp) :: y
      ! Representative nonlinear pointwise work (EOS-like Horner polynomial).
      y = ((0.5_wp*x - 1.25_wp)*x + 3.0_wp)*x - 0.75_wp
   end function poly

end module helper_mod
