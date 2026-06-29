!! p05 registry TU — an array of derived types, each owning an allocatable
!! field (mirrors a tracer registry: an array of per-tracer fields). Deep-dereferencing
!! `regs(t)%v` INSIDE a `do concurrent` defeats NVHPC device codegen; the fix
!! is the outer-shim — deref on the host, pass the leaf array to a flat kernel.
module registry_mod
   use dcp_kinds, only: wp
   implicit none
   private
   public :: col_t

   type :: col_t
      real(wp), allocatable :: v(:)
   end type col_t

end module registry_mod
