#include "dcp_directives.h"
!! p07 driver — sum a field on device, compare to the host-serial sum.
program p07_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, dcp_report
   use kernel_mod, only: run_kernel
   implicit none
   type(dcp_args_t) :: args
   real(wp), allocatable :: a(:)
   integer :: i, it
   integer(int64) :: t0
   real(wp) :: secs, s, ref, err, rtol
   real(wp), parameter :: REL = 1.0e-10_wp

   call dcp_get_args(args)
   allocate (a(args%n))
   do i = 1, args%n
      a(i) = real(modulo(i, 97), wp)*0.013_wp
   end do
   ref = 0.0_wp
   do i = 1, args%n
      ref = ref + a(i)
   end do

   call run_kernel(args%n, a, s)        ! warm-up
   t0 = dcp_tick()
   do it = 1, args%iters
      call run_kernel(args%n, a, s)
   end do
   secs = dcp_tock(t0)

   rtol = REL*max(abs(ref), 1.0_wp)
   err = abs(s - ref)
   call dcp_report("p07_reduction", DCP_DATA_NAME, args%n, args%iters, err, rtol, secs)
end program p07_main
