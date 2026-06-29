#include "dcp_directives.h"
!! kappa_shear model driver — solve the per-column diffusivity on device and
!! compare to a host-serial Picard solve (same fixed point). RESULT = max error.
program kappa_shear_main
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   use dcp_harness, only: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, &
                          dcp_max_abs_err, dcp_report
   use kernel_mod, only: kappa_shear_columns
   implicit none
   integer, parameter :: NZ = 64, MAXIT = 200
   real(wp), parameter :: ITOL = 1.0e-12_wp
   type(dcp_args_t) :: args
   real(wp), allocatable :: shear(:, :, :), n2(:, :, :), kappa(:, :, :), ref(:, :, :)
   integer :: nx, ny, i, j, k, it
   integer(int64) :: t0
   real(wp) :: secs, err, a, b, knew, kold

   call dcp_get_args(args)
   ny = 64
   nx = max(args%n/(ny*NZ), 1)
   allocate (shear(nx, ny, NZ), n2(nx, ny, NZ), kappa(nx, ny, NZ), ref(nx, ny, NZ))
   do k = 1, NZ
      do j = 1, ny
         do i = 1, nx
            shear(i, j, k) = real(modulo(i + 2*j + 3*k, 41), wp)*0.05_wp
            n2(i, j, k) = real(modulo(i + j + k, 23), wp)*0.02_wp
         end do
      end do
   end do
   ! host-serial reference (same Picard iteration)
   do k = 1, NZ
      do j = 1, ny
         do i = 1, nx
            a = 0.1_wp*abs(shear(i, j, k))
            b = 10.0_wp*abs(n2(i, j, k)) + 1.0_wp
            kold = a; knew = a
            do it = 1, MAXIT
               knew = a/(1.0_wp + b*kold*kold)
               if (abs(knew - kold) < ITOL) exit
               kold = knew
            end do
            ref(i, j, k) = knew
         end do
      end do
   end do

   call kappa_shear_columns(nx, ny, NZ, shear, n2, kappa)    ! warm-up
   t0 = dcp_tick()
   do it = 1, args%iters
      call kappa_shear_columns(nx, ny, NZ, shear, n2, kappa)
   end do
   secs = dcp_tock(t0)

   err = dcp_max_abs_err(reshape(kappa, [nx*ny*NZ]), reshape(ref, [nx*ny*NZ]))
   call dcp_report("kappa_shear", DCP_DATA_NAME, nx*ny*NZ, args%iters, err, 1.0e-7_wp, secs)
end program kappa_shear_main
