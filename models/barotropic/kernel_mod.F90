#include "dcp_directives.h"
!! barotropic model kernel — a split-explicit forward-backward gravity-wave fast
!! loop on a SMALL periodic 2D grid: each substep is two tiny DC kernels (update
!! u,v from grad eta; then eta from div u,v), data kept resident across all
!! substeps. Archetype: LAUNCH-BOUND — with many small substeps, kernel-launch
!! overhead dominates (the regime the p11 async/graph wrap targets). Σeta is
!! conserved (divergence form, periodic).
module kernel_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: barotropic_loop

contains

   subroutine barotropic_loop(nx, ny, nsub, eta, u, v, gdt, hdt)
      integer, intent(in) :: nx, ny, nsub
      real(wp), intent(inout) :: eta(nx, ny), u(nx, ny), v(nx, ny)
      real(wp), intent(in) :: gdt, hdt
      integer :: i, j, s, ip, jp, im, jm
      DCP_ENTER(eta)
      DCP_ENTER(u)
      DCP_ENTER(v)
      do s = 1, nsub
         do concurrent(j=1:ny, i=1:nx) local(ip, jp)
            ip = i + 1; if (ip > nx) ip = 1
            jp = j + 1; if (jp > ny) jp = 1
            u(i, j) = u(i, j) - gdt*(eta(ip, j) - eta(i, j))
            v(i, j) = v(i, j) - gdt*(eta(i, jp) - eta(i, j))
         end do
         do concurrent(j=1:ny, i=1:nx) local(im, jm)
            im = i - 1; if (im < 1) im = nx
            jm = j - 1; if (jm < 1) jm = ny
            eta(i, j) = eta(i, j) - hdt*((u(i, j) - u(im, j)) + (v(i, j) - v(i, jm)))
         end do
      end do
      DCP_UPDATE_SELF(eta)
      DCP_EXIT(eta)
      DCP_EXIT(u)
      DCP_EXIT(v)
   end subroutine barotropic_loop

end module kernel_mod
