#include "dcp_directives.h"
!! remap model driver — remap a profile from a uniform source grid to a
!! non-uniform target grid of equal column height; the RESULT metric is
!! per-column conservation of Σ q·dz (max over columns), to round-off.
program remap_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, dcp_report
   use kernel_mod, only: remap_columns
   implicit none
   integer, parameter :: NZ = 64
   real(wp), parameter :: HCOL = 64.0_wp
   type(dcp_args_t) :: args
   real(wp), allocatable :: dz_src(:, :, :), dz_tgt(:, :, :), q_src(:, :, :), q_tgt(:, :, :)
   integer :: nx, ny, i, j, k, it
   integer(int64) :: t0
   real(wp) :: secs, sgn, isrc, itgt, err, maxerr, rtol

   call dcp_get_args(args)
   ny = 64
   nx = max(args%n/(ny*NZ), 1)
   allocate (dz_src(nx, ny, NZ), dz_tgt(nx, ny, NZ), q_src(nx, ny, NZ), q_tgt(nx, ny, NZ))
   do k = 1, NZ
      sgn = real(2*modulo(k, 2) - 1, wp)            ! +/-1 alternating -> sums to NZ
      do j = 1, ny
         do i = 1, nx
            dz_src(i, j, k) = HCOL/real(NZ, wp)
            dz_tgt(i, j, k) = (HCOL/real(NZ, wp))*(1.0_wp + 0.4_wp*sgn)
            q_src(i, j, k) = real(modulo(i + j + k, 53), wp)*0.07_wp
         end do
      end do
   end do

   call remap_columns(nx, ny, NZ, dz_src, dz_tgt, q_src, q_tgt)   ! warm-up
   t0 = dcp_tick()
   do it = 1, args%iters
      call remap_columns(nx, ny, NZ, dz_src, dz_tgt, q_src, q_tgt)
   end do
   secs = dcp_tock(t0)

   ! per-column conservation: max |Σ q_tgt·dz_tgt − Σ q_src·dz_src|
   maxerr = 0.0_wp
   do j = 1, ny
      do i = 1, nx
         isrc = 0.0_wp; itgt = 0.0_wp
         do k = 1, NZ
            isrc = isrc + q_src(i, j, k)*dz_src(i, j, k)
            itgt = itgt + q_tgt(i, j, k)*dz_tgt(i, j, k)
         end do
         err = abs(itgt - isrc)
         if (err > maxerr) maxerr = err
      end do
   end do
   rtol = 1.0e-10_wp*HCOL
   call dcp_report("remap", DCP_DATA_NAME, nx*ny*NZ, args%iters, maxerr, rtol, secs)
end program remap_main
