#include "dcp_directives.h"
!! p07 kernel TU — a sum reduction over a field. The portable form is the
!! F2023 `do concurrent (...) reduce(+:s)` locality spec (NVHPC stdpar lowers it
!! to a device reduction); the result scalar returns via the data layer. Mirrors
!! CFL / mass-budget reductions. (If a compiler lacks `reduce`, this is the one
!! pattern that falls back to a `!$acc/!$omp ... reduction(+:)` directive.)
module kernel_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: run_kernel

contains

   subroutine run_kernel(n, a, s)
      integer, intent(in) :: n
      real(wp), intent(in) :: a(n)
      real(wp), intent(out) :: s
      integer :: i
      s = 0.0_wp
      DCP_ENTER(a)
      do concurrent(i=1:n) reduce(+:s)
         s = s + a(i)
      end do
      DCP_EXIT(a)
   end subroutine run_kernel

end module kernel_mod
