#include "dcp_directives.h"
!! p05 driver — the OUTER SHIM: loop the registry on the host, dereferencing
!! `regs(t)%v` there, and hand each leaf array to the flat device kernel.
program p05_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use registry_mod, only: col_t
   use kernel_mod, only: flat_scale
   implicit none
   integer, parameter :: NT = 8
   real(wp), parameter :: FAC = 1.5_wp
   type(dcp_args_t) :: args
   type(col_t) :: regs(NT)
   real(wp), allocatable :: ref(:)
   integer :: t, i, it, npt
   integer(int64) :: t0
   real(wp) :: secs, err

   call dcp_get_args(args)
   npt = max(args%n/NT, 1)
   allocate (ref(npt))
   do t = 1, NT
      allocate (regs(t)%v(npt))
      do i = 1, npt
         regs(t)%v(i) = real(modulo(i + t, 97), wp)*0.013_wp
      end do
   end do
   do i = 1, npt
      ref(i) = regs(1)%v(i)*FAC + 1.0_wp     ! per-element transform (tracer 1)
   end do

   ! warm-up + timed: shim derefs regs(t)%v on the host, flat kernel on device
   do t = 1, NT
      call flat_scale(npt, regs(t)%v, FAC)
   end do
   t0 = dcp_tick()
   do it = 1, args%iters
      do t = 1, NT
         ! reset so the repeated transform is comparable each iter
         do i = 1, npt
            regs(t)%v(i) = real(modulo(i + t, 97), wp)*0.013_wp
         end do
         call flat_scale(npt, regs(t)%v, FAC)
      end do
   end do
   secs = dcp_tock(t0)

   err = dcp_max_abs_err(regs(1)%v, ref)
   call dcp_report("p05_array_of_dt_shim", DCP_DATA_NAME, npt*NT, args%iters, err, &
                   1.0e-12_wp, secs)
end program p05_main
