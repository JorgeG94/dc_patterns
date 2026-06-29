#include "dcp_directives.h"
!! p04 type TU — a derived type with allocatable components, plus its
!! enter/exit-data helpers. Mapping rule (load-bearing): map the PARENT struct
!! BEFORE its components, and detach in reverse. Mirrors mapping a composed
!! state derived type with allocatable field components.
module type_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: field_t, field_enter, field_exit_update

   type :: field_t
      integer :: n = 0
      real(wp), allocatable :: a(:), b(:)
   end type field_t

contains

   subroutine field_enter(f)
      type(field_t), intent(inout) :: f
      DCP_ENTER(f)            ! parent first
      DCP_ENTER(f%a)         ! then components
      DCP_CREATE(f%b)
   end subroutine field_enter

   subroutine field_exit_update(f)
      type(field_t), intent(inout) :: f
      DCP_UPDATE_SELF(f%b)   ! pull results
      DCP_EXIT(f%b)          ! detach components first
      DCP_EXIT(f%a)
      DCP_EXIT(f)            ! parent last
   end subroutine field_exit_update

end module type_mod
