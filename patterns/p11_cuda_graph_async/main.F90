#include "dcp_directives.h"
!! p11 driver — accumulate b += a over NREP tiny async-wrapped kernels; check
!! b == NREP*a. The launch-bound regime where graph capture (NVHPC-acc) helps.
program p11_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use kernel_mod, only: many_small
   implicit none
   integer, parameter :: NREP = 50
   type(dcp_args_t) :: args
   real(wp), allocatable :: a(:), b(:), ref(:)
   integer :: i, it
   integer(int64) :: t0
   real(wp) :: secs, err

   call dcp_get_args(args)
   allocate (a(args%n), b(args%n), ref(args%n))
   do i = 1, args%n
      a(i) = real(modulo(i, 97), wp)*0.013_wp
      ref(i) = real(NREP, wp)*a(i)
   end do

   b = 0.0_wp
   call many_small(args%n, NREP, a, b)        ! warm-up
   t0 = dcp_tick()
   do it = 1, args%iters
      b = 0.0_wp
      call many_small(args%n, NREP, a, b)
   end do
   secs = dcp_tock(t0)

   err = dcp_max_abs_err(b, ref)
   call dcp_report("p11_cuda_graph_async", DCP_DATA_NAME, args%n, args%iters*NREP, &
                   err, 1.0e-12_wp, secs)
end program p11_main
