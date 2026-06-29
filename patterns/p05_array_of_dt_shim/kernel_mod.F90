#include "dcp_directives.h"
!! p05 kernel TU — the flat-impl device kernel. It takes a PLAIN explicit-shape
!! array (already dereferenced from the registry on the host) — no derived type,
!! no array-of-DT indirection reaches the device.
module kernel_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: flat_scale

contains

   subroutine flat_scale(n, v, f)
      integer, intent(in) :: n
      real(wp), intent(inout) :: v(n)
      real(wp), intent(in) :: f
      integer :: i
      DCP_ENTER(v)
      do concurrent(i=1:n)
         v(i) = v(i)*f + 1.0_wp
      end do
      DCP_UPDATE_SELF(v)
      DCP_EXIT(v)
   end subroutine flat_scale

end module kernel_mod
