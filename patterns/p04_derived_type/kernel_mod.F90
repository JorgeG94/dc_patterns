#include "dcp_directives.h"
!! p04 kernel TU — a bare DC that dereferences allocatable COMPONENTS of a
!! derived type (f%a, f%b) on the device. Works only because the parent + its
!! components were mapped (field_enter); the loop bound is copied to a local
!! scalar so it isn't re-derefed from the struct per launch.
module kernel_mod
   use dcp_kinds, only: wp
   use type_mod, only: field_t
   implicit none
   private
   public :: apply

contains

   subroutine apply(f)
      type(field_t), intent(inout) :: f
      integer :: i, n
      n = f%n
      do concurrent(i=1:n)
         f%b(i) = 2.0_wp*f%a(i) + 1.0_wp
      end do
   end subroutine apply

end module kernel_mod
