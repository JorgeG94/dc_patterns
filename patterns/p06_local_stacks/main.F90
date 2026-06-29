#include "dcp_directives.h"
!! p06 driver — per-column cumulative integral on device vs host-serial ref.
program p06_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use kernel_mod, only: column_integral
   implicit none
   integer, parameter :: NZ = 64
   type(dcp_args_t) :: args
   real(wp), allocatable :: a(:, :, :), b(:, :, :), ref(:, :, :)
   integer :: nx, ny, i, j, k, it
   integer(int64) :: t0
   real(wp) :: secs, err
   real(wp), parameter :: TOL = 1.0e-9_wp

   call dcp_get_args(args)
   ny = 64
   nx = max(args%n/(ny*NZ), 1)
   allocate (a(nx, ny, NZ), b(nx, ny, NZ), ref(nx, ny, NZ))
   do concurrent(k=1:NZ, j=1:ny, i=1:nx)
      a(i, j, k) = real(modulo(i + j + k, 97), wp)*0.013_wp
   end do
   ! host-serial reference column integral
   do j = 1, ny
      do i = 1, nx
         ref(i, j, 1) = a(i, j, 1)
         do k = 2, NZ
            ref(i, j, k) = ref(i, j, k - 1) + a(i, j, k)
         end do
      end do
   end do

   call column_integral(nx, ny, NZ, a, b)     ! warm-up
   t0 = dcp_tick()
   do it = 1, args%iters
      call column_integral(nx, ny, NZ, a, b)
   end do
   secs = dcp_tock(t0)
   err = dcp_max_abs_err(reshape(b, [nx*ny*NZ]), reshape(ref, [nx*ny*NZ]))
   call dcp_report("p06_local_stacks", DCP_DATA_NAME, nx*ny*NZ, args%iters, err, TOL, secs)
end program p06_main
