#include "dcp_directives.h"
!! p03 driver — time both index orders over the same 3D field; emit a RESULT
!! line for each (k,j,i vs i,j,k) so the loop-order penalty shows per backend.
program p03_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use kernel_mod, only: stencil_kji, stencil_ijk
   implicit none
   integer, parameter :: NY = 32, NZ = 32
   type(dcp_args_t) :: args
   real(wp), allocatable :: a(:, :, :), b(:, :, :), ref(:, :, :)
   integer :: nx, i, j, k, it
   integer(int64) :: t0
   real(wp) :: secs, err
   real(wp), parameter :: TOL = 1.0e-12_wp

   call dcp_get_args(args)
   nx = max(args%n/(NY*NZ), 1)
   allocate (a(nx, NY, NZ), b(nx, NY, NZ), ref(nx, NY, NZ))
   do concurrent(k=1:NZ, j=1:NY, i=1:nx)
      a(i, j, k) = real(modulo(i + j + k, 97), wp)*0.013_wp
   end do
   ref = 2.0_wp*a + 1.0_wp

   call stencil_kji(nx, NY, NZ, a, b)
   t0 = dcp_tick()
   do it = 1, args%iters
      call stencil_kji(nx, NY, NZ, a, b)
   end do
   secs = dcp_tock(t0)
   err = dcp_max_abs_err(reshape(b, [nx*NY*NZ]), reshape(ref, [nx*NY*NZ]))
   call dcp_report("p03_kji", DCP_DATA_NAME, nx*NY*NZ, args%iters, err, TOL, secs)

   call stencil_ijk(nx, NY, NZ, a, b)
   t0 = dcp_tick()
   do it = 1, args%iters
      call stencil_ijk(nx, NY, NZ, a, b)
   end do
   secs = dcp_tock(t0)
   err = dcp_max_abs_err(reshape(b, [nx*NY*NZ]), reshape(ref, [nx*NY*NZ]))
   call dcp_report("p03_ijk", DCP_DATA_NAME, nx*NY*NZ, args%iters, err, TOL, secs)
end program p03_main
