#include "dcp_directives.h"
!! p10 driver — run the uniform-select kernel (mode=2) and compare to ref.
program p10_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use kernel_mod, only: apply_mode
   implicit none
   integer, parameter :: MODE = 2
   type(dcp_args_t) :: args
   real(wp), allocatable :: a(:), b(:), ref(:)
   integer :: i, it
   integer(int64) :: t0
   real(wp) :: secs, err

   call dcp_get_args(args)
   allocate (a(args%n), b(args%n), ref(args%n))
   do i = 1, args%n
      a(i) = real(modulo(i, 97), wp)*0.013_wp
      ref(i) = 2.0_wp*a(i)        ! mode 2
   end do

   call apply_mode(args%n, MODE, a, b)     ! warm-up
   t0 = dcp_tick()
   do it = 1, args%iters
      call apply_mode(args%n, MODE, a, b)
   end do
   secs = dcp_tock(t0)

   err = dcp_max_abs_err(b, ref)
   call dcp_report("p10_uniform_select", DCP_DATA_NAME, args%n, args%iters, &
                   err, 1.0e-12_wp, secs)
end program p10_main
