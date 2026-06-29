#include "dcp_directives.h"
!! p01 driver TU — builds input, computes a host-serial reference (same `poly`),
!! runs the device kernel `iters` times, compares, and prints the RESULT line.
program p01_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use kernel_mod, only: run_kernel
   use helper_mod, only: poly
   implicit none
   type(dcp_args_t) :: args
   real(wp), allocatable :: a(:), b(:), ref(:)
   integer :: i, it
   integer(int64) :: t0
   real(wp) :: secs, err
   real(wp), parameter :: TOL = 1.0e-12_wp

   call dcp_get_args(args)
   allocate (a(args%n), b(args%n), ref(args%n))
   do i = 1, args%n
      a(i) = real(modulo(i, 97), wp)*0.013_wp
   end do
   ! Host-serial reference (poly is pure; DCP_DECLARE_TARGET is inert on host).
   do i = 1, args%n
      ref(i) = poly(a(i))
   end do

   call run_kernel(args%n, a, b)        ! warm-up (discarded)
   t0 = dcp_tick()
   do it = 1, args%iters
      call run_kernel(args%n, a, b)
   end do
   secs = dcp_tock(t0)

   err = dcp_max_abs_err(b, ref)
   call dcp_report("p01_acc_routine_xtu", DCP_DATA_NAME, args%n, args%iters, &
                   err, TOL, secs)
end program p01_main
