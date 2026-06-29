#include "dcp_directives.h"
!! p09 driver — ensure the workspace once, run many steps reusing it, compare.
program p09_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use kernel_mod, only: ws_ensure, ws_cleanup, step
   implicit none
   type(dcp_args_t) :: args
   real(wp), allocatable :: a(:), b(:), ref(:)
   integer :: i, it
   integer(int64) :: t0
   real(wp) :: secs, err

   call dcp_get_args(args)
   allocate (a(args%n), b(args%n), ref(args%n))
   do i = 1, args%n
      a(i) = real(modulo(i, 97), wp)*0.013_wp
   end do
   do i = 1, args%n
      ref(i) = a(i)*a(i) + 1.0_wp
   end do

   call ws_ensure(args%n)        ! map workspace ONCE
   call step(args%n, a, b)        ! warm-up
   t0 = dcp_tick()
   do it = 1, args%iters
      call step(args%n, a, b)
   end do
   secs = dcp_tock(t0)
   call ws_cleanup()

   err = dcp_max_abs_err(b, ref)
   call dcp_report("p09_persistent_workspace", DCP_DATA_NAME, args%n, args%iters, &
                   err, 1.0e-12_wp, secs)
end program p09_main
