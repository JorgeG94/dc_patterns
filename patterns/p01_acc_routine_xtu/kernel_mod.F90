#include "dcp_directives.h"
!! p01 kernel TU — bare `do concurrent` compute that calls the cross-module
!! device routine `poly`. Data residency is the only decorated part: map the
!! input in, allocate the output on device, run the DC, copy the output back.
!! Explicit-shape dummies (a(n), b(n)) — the descriptor-friendly flat form.
module kernel_mod
   use dcp_kinds, only: wp
   use helper_mod, only: poly
   implicit none
   private
   public :: run_kernel

contains

   subroutine run_kernel(n, a, b)
      integer, intent(in) :: n
      real(wp), intent(in) :: a(n)
      real(wp), intent(out) :: b(n)
      integer :: i
      DCP_ENTER(a)
      DCP_CREATE(b)
      do concurrent(i=1:n)
         b(i) = poly(a(i))
      end do
      DCP_UPDATE_SELF(b)
      DCP_EXIT(a)
      DCP_EXIT(b)
   end subroutine run_kernel

end module kernel_mod
