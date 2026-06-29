#include "dcp_directives.h"
!! continuity model driver — advect a bump around a periodic 1D domain for
!! `iters` steps; the RESULT metric is MASS CONSERVATION (Σh invariant to
!! round-off for a flux-form update), checked via the on-device reduction.
program continuity_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, dcp_report
   use kernel_mod, only: continuity_step, mass_sum, flux_workspace_ensure, flux_cleanup
   implicit none
   type(dcp_args_t) :: args
   real(wp), allocatable :: h(:), hnew(:)
   integer :: n, i, s
   integer(int64) :: t0
   real(wp) :: secs, u, dtdx, m0, mfin, err, rtol
   real(wp), parameter :: REL = 1.0e-11_wp   !! relative mass-conservation tol

   call dcp_get_args(args)
   n = args%n
   allocate (h(n), hnew(n))
   do i = 1, n
      h(i) = 1.0_wp + 0.5_wp*exp(-((real(i - n/2, wp))/real(n/16, wp))**2)
   end do
   u = 1.0_wp
   dtdx = 0.5_wp     ! CFL = u*dt/dx = 0.5

   m0 = 0.0_wp
   do i = 1, n
      m0 = m0 + h(i)
   end do

   call flux_workspace_ensure(n)
   t0 = dcp_tick()
   do s = 1, args%iters
      call continuity_step(n, h, hnew, u, dtdx)
      h = hnew
   end do
   secs = dcp_tock(t0)

   mfin = mass_sum(n, h)          ! on-device reduction
   err = abs(mfin - m0)
   rtol = REL*max(abs(m0), 1.0_wp)
   call dcp_report("continuity", DCP_DATA_NAME, n, args%iters, err, rtol, secs)
   call flux_cleanup()
end program continuity_main
