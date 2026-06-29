!! Working precision + a couple of integer kinds, dependency-free.
module dcp_kinds
   use, intrinsic :: iso_fortran_env, only: real64, int64
   implicit none
   public
   integer, parameter :: wp = real64
   integer, parameter :: MAX_NZ = 128
      !! Fixed upper bound for per-column `local()` stack arrays in DC kernels
      !! (dummy-sized automatics crash NVHPC stdpar device codegen).
end module dcp_kinds
