#include "dcp_directives.h"
!! hor_visc model kernel — Smagorinsky horizontal viscosity on a periodic 2D
!! field: DC1 builds the flow-aware viscosity nu(i,j) from a 5-point u/v stencil
!! (cross-module helper) into a persistent workspace; DC2 applies the
!! variable-coefficient Laplacian to u in FLUX form (snapshot into unew), so the
!! domain integral of u is conserved. Archetype: compute-heavier stencil.
module kernel_mod
   use dcp_kinds, only: wp
   use helper_mod, only: smag_nu
   implicit none
   private
   public :: hv_ensure, hv_cleanup, hor_visc_step

   real(wp), allocatable, save :: nu(:, :)   !! persistent cell-centre viscosity

contains

   subroutine hv_ensure(nx, ny)
      integer, intent(in) :: nx, ny
      if (allocated(nu)) then
         if (size(nu, 1) == nx .and. size(nu, 2) == ny) return
         DCP_EXIT(nu)
         deallocate (nu)
      end if
      allocate (nu(nx, ny))
      DCP_CREATE(nu)
   end subroutine hv_ensure

   subroutine hv_cleanup()
      if (allocated(nu)) then
         DCP_EXIT(nu)
         deallocate (nu)
      end if
   end subroutine hv_cleanup

   subroutine hor_visc_step(nx, ny, u, v, unew, cs2dx2, coef)
      integer, intent(in) :: nx, ny
      real(wp), intent(in)  :: u(nx, ny), v(nx, ny)
      real(wp), intent(out) :: unew(nx, ny)
      real(wp), intent(in)  :: cs2dx2, coef
      integer :: i, j, ip, im, jp, jm
      real(wp) :: dudx, dudy, dvdx, dvdy, fe, fw, fn, fs
      DCP_ENTER(u)
      DCP_ENTER(v)
      DCP_CREATE(unew)
      ! DC1: flow-aware viscosity from the centred stencil
      do concurrent(j=1:ny, i=1:nx) local(ip, im, jp, jm, dudx, dudy, dvdx, dvdy)
         ip = i + 1; if (ip > nx) ip = 1
         im = i - 1; if (im < 1) im = nx
         jp = j + 1; if (jp > ny) jp = 1
         jm = j - 1; if (jm < 1) jm = ny
         dudx = 0.5_wp*(u(ip, j) - u(im, j))
         dudy = 0.5_wp*(u(i, jp) - u(i, jm))
         dvdx = 0.5_wp*(v(ip, j) - v(im, j))
         dvdy = 0.5_wp*(v(i, jp) - v(i, jm))
         nu(i, j) = smag_nu(dudx, dudy, dvdx, dvdy, cs2dx2)
      end do
      ! DC2: flux-form variable-coefficient Laplacian of u (conservative)
      do concurrent(j=1:ny, i=1:nx) local(ip, im, jp, jm, fe, fw, fn, fs)
         ip = i + 1; if (ip > nx) ip = 1
         im = i - 1; if (im < 1) im = nx
         jp = j + 1; if (jp > ny) jp = 1
         jm = j - 1; if (jm < 1) jm = ny
         fe = 0.5_wp*(nu(ip, j) + nu(i, j))*(u(ip, j) - u(i, j))
         fw = 0.5_wp*(nu(i, j) + nu(im, j))*(u(i, j) - u(im, j))
         fn = 0.5_wp*(nu(i, jp) + nu(i, j))*(u(i, jp) - u(i, j))
         fs = 0.5_wp*(nu(i, j) + nu(i, jm))*(u(i, j) - u(i, jm))
         unew(i, j) = u(i, j) + coef*((fe - fw) + (fn - fs))
      end do
      DCP_UPDATE_SELF(unew)
      DCP_EXIT(u)
      DCP_EXIT(v)
      DCP_EXIT(unew)
   end subroutine hor_visc_step

end module kernel_mod
