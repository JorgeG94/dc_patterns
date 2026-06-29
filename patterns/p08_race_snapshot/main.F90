#include "dcp_directives.h"
!! p08 driver — run NSWEEP smoothing sweeps the snapshot way (ping-pong buffers)
!! and compare to a host-serial snapshot reference: that is the RESULT (the
!! portable, correct pattern). Then run the in-place (racy) version and print an
!! INFO line with its drift from the reference — ~0 on GPU "by luck", nonzero on
!! a serial backend. The INFO line never fails the run.
program p08_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use kernel_mod, only: sweep_snapshot, sweep_inplace
   implicit none
   integer, parameter :: NSWEEP = 5
   type(dcp_args_t) :: args
   real(wp), allocatable :: a0(:), b1(:), b2(:), prev(:), ref(:), c(:)
   integer :: n, i, s, it
   integer(int64) :: t0
   real(wp) :: secs, err, info

   call dcp_get_args(args)
   n = args%n
   allocate (a0(n), b1(n), b2(n), prev(n), ref(n), c(n))
   do i = 1, n
      a0(i) = real(modulo(i, 97), wp)*0.013_wp
   end do

   ! host-serial snapshot reference (each sweep reads the previous sweep)
   ref = a0
   do s = 1, NSWEEP
      prev = ref
      do i = 2, n - 1
         ref(i) = 0.25_wp*prev(i - 1) + 0.5_wp*prev(i) + 0.25_wp*prev(i + 1)
      end do
   end do

   ! device snapshot ping-pong (timed)
   t0 = dcp_tick()
   do it = 1, args%iters
      b1 = a0
      do s = 1, NSWEEP
         call sweep_snapshot(n, b1, b2)
         b1 = b2
      end do
   end do
   secs = dcp_tock(t0)
   err = dcp_max_abs_err(b1, ref)
   call dcp_report("p08_race_snapshot", DCP_DATA_NAME, n, args%iters*NSWEEP, err, &
                   1.0e-9_wp, secs)

   ! device in-place (racy) — informational drift from the reference
   c = a0
   do s = 1, NSWEEP
      call sweep_inplace(n, c)
   end do
   info = dcp_max_abs_err(c, ref)
   write (*, "(A,ES13.6)") "INFO p08_inplace_race data="//DCP_DATA_NAME// &
      " max_abs_drift_vs_ref=", info
end program p08_main
