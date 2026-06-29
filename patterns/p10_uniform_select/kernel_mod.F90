#include "dcp_directives.h"
!! p10 kernel TU — a loop-invariant `select case (mode)` kept INSIDE one DC.
!! `mode` is uniform across the whole launch, so the branch is ~free; splitting
!! into one DC per case would instead double the kernel launches. Mirrors the
!! vcoord / remap-scheme dispatch kept inside a single kernel.
module kernel_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: apply_mode

contains

   subroutine apply_mode(n, mode, a, b)
      integer, intent(in) :: n, mode
      real(wp), intent(in)  :: a(n)
      real(wp), intent(out) :: b(n)
      integer :: i
      DCP_ENTER(a)
      DCP_CREATE(b)
      do concurrent(i=1:n)
         select case (mode)         ! loop-invariant; one launch, not one-per-case
         case (1)
            b(i) = a(i) + 1.0_wp
         case (2)
            b(i) = 2.0_wp*a(i)
         case default
            b(i) = a(i)*a(i)
         end select
      end do
      DCP_UPDATE_SELF(b)
      DCP_EXIT(a)
      DCP_EXIT(b)
   end subroutine apply_mode

end module kernel_mod
