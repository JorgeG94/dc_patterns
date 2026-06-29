#include "dcp_directives.h"
!! p08 kernel TU — a 1-2-1 smoothing sweep, two ways:
!!   * snapshot (correct, portable): read `ain`, write a SEPARATE `aout` — no
!!     iteration writes a cell another reads, so the DC is race-free.
!!   * in-place (the anti-pattern): read neighbours of `a` while writing `a(i)`
!!     — a read-write race; "passes by luck" on GPU, drifts on a serial backend.
module kernel_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: sweep_snapshot, sweep_inplace

contains

   subroutine sweep_snapshot(n, ain, aout)
      integer, intent(in) :: n
      real(wp), intent(in)  :: ain(n)
      real(wp), intent(out) :: aout(n)
      integer :: i
      DCP_ENTER(ain)
      DCP_CREATE(aout)
      do concurrent(i=1:n)
         if (i == 1 .or. i == n) then
            aout(i) = ain(i)
         else
            aout(i) = 0.25_wp*ain(i - 1) + 0.5_wp*ain(i) + 0.25_wp*ain(i + 1)
         end if
      end do
      DCP_UPDATE_SELF(aout)
      DCP_EXIT(ain)
      DCP_EXIT(aout)
   end subroutine sweep_snapshot

   subroutine sweep_inplace(n, a)
      integer, intent(in) :: n
      real(wp), intent(inout) :: a(n)
      integer :: i
      DCP_ENTER(a)
      do concurrent(i=2:n - 1)
         a(i) = 0.25_wp*a(i - 1) + 0.5_wp*a(i) + 0.25_wp*a(i + 1)   ! RACE
      end do
      DCP_UPDATE_SELF(a)
      DCP_EXIT(a)
   end subroutine sweep_inplace

end module kernel_mod
