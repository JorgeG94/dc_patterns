#include "dcp_directives.h"
!! hor_visc model driver — apply Smagorinsky viscosity to a periodic 2D field
!! for `iters` steps; the RESULT metric is conservation of Σu (the flux-form
!! Laplacian preserves the domain integral), to round-off.
program hor_visc_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, dcp_report
   use kernel_mod, only: hv_ensure, hv_cleanup, hor_visc_step
   implicit none
   integer, parameter :: NY = 512
   real(wp), parameter :: CS2DX2 = 0.01_wp, COEF = 0.1_wp
   type(dcp_args_t) :: args
   real(wp), allocatable :: u(:, :), v(:, :), unew(:, :)
   integer :: nx, i, j, it
   integer(int64) :: t0
   real(wp) :: secs, s0, sfin, err, rtol

   call dcp_get_args(args)
   nx = max(args%n/NY, 1)
   allocate (u(nx, NY), v(nx, NY), unew(nx, NY))
   do j = 1, NY
      do i = 1, nx
         u(i, j) = sin(real(i, wp)*0.05_wp) + 0.3_wp*cos(real(j, wp)*0.04_wp)
         v(i, j) = cos(real(i, wp)*0.03_wp) - 0.2_wp*sin(real(j, wp)*0.06_wp)
      end do
   end do
   s0 = sum(u)

   call hv_ensure(nx, NY)
   call hor_visc_step(nx, NY, u, v, unew, CS2DX2, COEF)    ! warm-up
   t0 = dcp_tick()
   do it = 1, args%iters
      call hor_visc_step(nx, NY, u, v, unew, CS2DX2, COEF)
      u = unew
   end do
   secs = dcp_tock(t0)
   call hv_cleanup()

   sfin = sum(u)
   err = abs(sfin - s0)
   rtol = 1.0e-9_wp*max(abs(s0), 1.0_wp)
   call dcp_report("hor_visc", DCP_DATA_NAME, nx*NY, args%iters, err, rtol, secs)
end program hor_visc_main
