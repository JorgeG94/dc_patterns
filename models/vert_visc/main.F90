#include "dcp_directives.h"
!! vert_visc model driver — diffuse a stratified column for `iters` backward-Euler
!! steps; the RESULT metric is per-column conservation of ΣT (no-flux diffusion
!! preserves the column integral), max over columns, to round-off.
program vert_visc_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, dcp_report
   use kernel_mod, only: vdiff_columns
   implicit none
   integer, parameter :: NZ = 64
   real(wp), parameter :: ALPHA = 0.25_wp
   type(dcp_args_t) :: args
   real(wp), allocatable :: t(:, :, :), s0(:, :)
   integer :: nx, ny, i, j, k, it
   integer(int64) :: t0
   real(wp) :: secs, ssum, err, maxerr, rtol

   call dcp_get_args(args)
   ny = 64
   nx = max(args%n/(ny*NZ), 1)
   allocate (t(nx, ny, NZ), s0(nx, ny))
   do k = 1, NZ
      do j = 1, ny
         do i = 1, nx
            t(i, j, k) = real(modulo(i + j, 17), wp) + real(k, wp)   ! stratified
         end do
      end do
   end do
   s0 = 0.0_wp
   do k = 1, NZ
      s0(:, :) = s0(:, :) + t(:, :, k)        ! initial column sums
   end do

   call vdiff_columns(nx, ny, NZ, t, ALPHA)    ! warm-up
   t0 = dcp_tick()
   do it = 1, args%iters
      call vdiff_columns(nx, ny, NZ, t, ALPHA)
   end do
   secs = dcp_tock(t0)

   maxerr = 0.0_wp
   do j = 1, ny
      do i = 1, nx
         ssum = 0.0_wp
         do k = 1, NZ
            ssum = ssum + t(i, j, k)
         end do
         err = abs(ssum - s0(i, j))
         if (err > maxerr) maxerr = err
      end do
   end do
   rtol = 1.0e-9_wp*real(NZ, wp)*20.0_wp
   call dcp_report("vert_visc", DCP_DATA_NAME, nx*ny*NZ, args%iters, maxerr, rtol, secs)
end program vert_visc_main
