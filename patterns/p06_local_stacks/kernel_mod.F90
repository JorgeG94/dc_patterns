#include "dcp_directives.h"
!! p06 kernel TU — per-column DC with a fixed-size `local()` stack array. Each
!! (i,j) thread owns a private `col(MAX_NZ)` scratch to do a serial
!! down-column transform (here a cumulative integral). The stack MUST be
!! fixed-size: a dummy-sized automatic crashes NVHPC stdpar device codegen
!! (CUDA_ERROR_ILLEGAL_ADDRESS). Mirrors per-column remap / vertical-mixing solves.
module kernel_mod
   use dcp_kinds, only: wp, MAX_NZ
   implicit none
   private
   public :: column_integral

contains

   subroutine column_integral(nx, ny, nz, a, b)
      integer, intent(in) :: nx, ny, nz
      real(wp), intent(in)  :: a(nx, ny, nz)
      real(wp), intent(out) :: b(nx, ny, nz)
      integer :: i, j, k
      real(wp) :: col(MAX_NZ)
      DCP_ENTER(a)
      DCP_CREATE(b)
      do concurrent(j=1:ny, i=1:nx) local(col, k)
         col(1) = a(i, j, 1)
         do k = 2, nz
            col(k) = col(k - 1) + a(i, j, k)   ! running column integral
         end do
         do k = 1, nz
            b(i, j, k) = col(k)
         end do
      end do
      DCP_UPDATE_SELF(b)
      DCP_EXIT(a)
      DCP_EXIT(b)
   end subroutine column_integral

end module kernel_mod
