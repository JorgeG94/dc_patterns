#include "dcp_directives.h"
!! p09 kernel TU — a module-level scratch workspace, allocated + mapped to the
!! device ONCE (lazy `*_ensure`) and reused across every step. The anti-pattern
!! is allocating scratch inside the hot kernel each call, which bridges
!! device-resident inputs with implicit per-call memcpys. Mirrors the
!! persistent PPM / hdiff / remap workspaces.
module kernel_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: ws_ensure, ws_cleanup, step

   real(wp), allocatable, save :: ws(:)   !! persistent device scratch

contains

   subroutine ws_ensure(n)
      integer, intent(in) :: n
      if (allocated(ws)) then
         if (size(ws) == n) return
         DCP_EXIT(ws)
         deallocate (ws)
      end if
      allocate (ws(n))
      DCP_CREATE(ws)          ! mapped ONCE, reused across all steps
   end subroutine ws_ensure

   subroutine ws_cleanup()
      if (allocated(ws)) then
         DCP_EXIT(ws)
         deallocate (ws)
      end if
   end subroutine ws_cleanup

   subroutine step(n, a, b)
      integer, intent(in) :: n
      real(wp), intent(in)  :: a(n)
      real(wp), intent(out) :: b(n)
      integer :: i
      DCP_ENTER(a)
      DCP_CREATE(b)
      do concurrent(i=1:n)
         ws(i) = a(i)*a(i)         ! scratch into the persistent workspace
      end do
      do concurrent(i=1:n)
         b(i) = ws(i) + 1.0_wp
      end do
      DCP_UPDATE_SELF(b)
      DCP_EXIT(a)
      DCP_EXIT(b)
   end subroutine step

end module kernel_mod
