#include "dcp_directives.h"
!! barotropic model driver — small fixed grid, many substeps per call (the
!! launch-bound regime; --n is ignored, --iters scales the outer reps). RESULT
!! metric is conservation of Σeta to round-off. mcells counts cell·substeps.
program barotropic_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, dcp_report
   use kernel_mod, only: barotropic_loop
   implicit none
   integer, parameter :: NX = 128, NY = 128, NSUB = 200
   real(wp), parameter :: GDT = 0.1_wp, HDT = 0.1_wp
   type(dcp_args_t) :: args
   real(wp), allocatable :: eta(:, :), u(:, :), v(:, :)
   integer :: i, j, it
   integer(int64) :: t0
   real(wp) :: secs, s0, sfin, err, rtol

   call dcp_get_args(args)
   allocate (eta(NX, NY), u(NX, NY), v(NX, NY))
   do j = 1, NY
      do i = 1, NX
         eta(i, j) = exp(-((real(i - NX/2, wp))**2 + (real(j - NY/2, wp))**2)/200.0_wp)
         u(i, j) = 0.0_wp
         v(i, j) = 0.0_wp
      end do
   end do
   s0 = sum(eta)

   call barotropic_loop(NX, NY, NSUB, eta, u, v, GDT, HDT)   ! warm-up
   ! reset state so each timed rep is identical
   t0 = dcp_tick()
   do it = 1, args%iters
      do j = 1, NY
         do i = 1, NX
            eta(i, j) = exp(-((real(i - NX/2, wp))**2 + (real(j - NY/2, wp))**2)/200.0_wp)
            u(i, j) = 0.0_wp
            v(i, j) = 0.0_wp
         end do
      end do
      call barotropic_loop(NX, NY, NSUB, eta, u, v, GDT, HDT)
   end do
   secs = dcp_tock(t0)

   sfin = sum(eta)
   err = abs(sfin - s0)
   rtol = 1.0e-9_wp*max(abs(s0), 1.0_wp)
   call dcp_report("barotropic", DCP_DATA_NAME, NX*NY*NSUB, args%iters, err, rtol, secs)
end program barotropic_main
