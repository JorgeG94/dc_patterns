#include "dcp_directives.h"
!! continuity model kernel — 1D periodic finite-volume thickness transport with
!! a limited (2nd-order MUSCL) face reconstruction, advection speed u>0. The
!! archetype is bandwidth-bound: read the field + neighbours, write fluxes, then
!! a snapshot update into hnew. `mass_sum` is the conserved-quantity reduction
!! (the CFL/budget reduction pattern). Persistent module flux workspace.
module kernel_mod
   use dcp_kinds, only: wp
   use helper_mod, only: vanleer
   implicit none
   private
   public :: continuity_step, mass_sum, flux_workspace_ensure, flux_cleanup

   real(wp), allocatable, save :: flux(:)   !! persistent face-flux workspace

contains

   subroutine flux_workspace_ensure(n)
      integer, intent(in) :: n
      if (allocated(flux)) then
         if (size(flux) == n) return
         deallocate (flux)
      end if
      allocate (flux(n))
      DCP_CREATE(flux)
   end subroutine flux_workspace_ensure

   subroutine flux_cleanup()
      if (allocated(flux)) then
         DCP_EXIT(flux)
         deallocate (flux)
      end if
   end subroutine flux_cleanup

   subroutine continuity_step(n, h, hnew, u, dtdx)
      integer, intent(in) :: n
      real(wp), intent(in)  :: h(n)
      real(wp), intent(out) :: hnew(n)
      real(wp), intent(in)  :: u, dtdx
      integer :: i, im1, im2, ip1
      real(wp) :: dnum, dden, r, hface
      DCP_ENTER(h)
      DCP_CREATE(hnew)
      ! face flux at the LEFT face of cell i (between i-1 and i), u>0 upwind
      do concurrent(i=1:n) local(im1, im2, dnum, dden, r, hface)
         im1 = i - 1; if (im1 < 1) im1 = n
         im2 = im1 - 1; if (im2 < 1) im2 = n
         dden = h(im1) - h(im2)
         dnum = h(i) - h(im1)
         r = 0.0_wp
         if (abs(dden) > 1.0e-30_wp) r = dnum/dden
         hface = h(im1) + 0.5_wp*vanleer(r)*dden
         flux(i) = u*hface
      end do
      ! snapshot update: hnew from h and the fluxes (no in-place race)
      do concurrent(i=1:n) local(ip1)
         ip1 = i + 1; if (ip1 > n) ip1 = 1
         hnew(i) = h(i) - dtdx*(flux(ip1) - flux(i))
      end do
      DCP_UPDATE_SELF(hnew)
      DCP_EXIT(h)
      DCP_EXIT(hnew)
   end subroutine continuity_step

   function mass_sum(n, h) result(s)
      integer, intent(in) :: n
      real(wp), intent(in) :: h(n)
      real(wp) :: s
      integer :: i
      s = 0.0_wp
      DCP_ENTER(h)
      do concurrent(i=1:n) reduce(+:s)
         s = s + h(i)
      end do
      DCP_EXIT(h)
   end function mass_sum

end module kernel_mod
