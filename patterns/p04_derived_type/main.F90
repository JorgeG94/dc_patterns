#include "dcp_directives.h"
!! p04 driver — map a derived type (parent + components), run the DC that
!! derefs its components on device, pull back and compare to a host reference.
program p04_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use type_mod, only: field_t, field_enter, field_exit_update
   use kernel_mod, only: apply
   implicit none
   type(dcp_args_t) :: args
   type(field_t) :: f
   real(wp), allocatable :: ref(:)
   integer :: i, it
   integer(int64) :: t0
   real(wp) :: secs, err

   call dcp_get_args(args)
   f%n = args%n
   allocate (f%a(f%n), f%b(f%n), ref(f%n))
   do i = 1, f%n
      f%a(i) = real(modulo(i, 97), wp)*0.013_wp
   end do
   do i = 1, f%n
      ref(i) = 2.0_wp*f%a(i) + 1.0_wp
   end do

   call field_enter(f)
   call apply(f)               ! warm-up (on device, components present)
   t0 = dcp_tick()
   do it = 1, args%iters
      call apply(f)
   end do
   secs = dcp_tock(t0)
   call field_exit_update(f)    ! pull f%b back, detach

   err = dcp_max_abs_err(f%b, ref)
   call dcp_report("p04_derived_type", DCP_DATA_NAME, f%n, args%iters, err, 1.0e-12_wp, secs)
end program p04_main
