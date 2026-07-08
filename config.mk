# dc_patterns/config.mk — the ONE file you edit per machine.
# Override on the CLI instead if you prefer:
#   make -C patterns/p01_acc_routine_xtu FC=ftn FC_FLAGS='...' DATA=acc MODFLAG='-J build' run
#
# Concept: the COMPUTE is always `do concurrent`. DATA selects only the
# device data-mapping / routine-declaration directive layer:
#   omp  -> OpenMP target-data (!$omp target enter data / declare target) — portable default
#   acc  -> OpenACC data       (!$acc enter data / routine seq)           — Cray/CCE; NVHPC cross-check
#   none -> no device data; bare DC on the host                          — gfortran/ifx serial reference
#
# FC_FLAGS must enable the chosen DATA layer for your compiler (see the
# commented variants below). MODFLAG is the per-compiler module-output flag.

DATA     ?= omp
GPU_ARCH ?= cc70

# ===========================================================================
# NVIDIA / NVHPC (nvfortran) — DC offloads to the GPU via -stdpar=gpu
# ===========================================================================
FC       ?= nvfortran
FC_FLAGS ?= -O2 -Kieee -stdpar=gpu -gpu=$(GPU_ARCH) -mp=gpu
MODFLAG  ?= -module build
#   DATA=omp  (default) : -O2 -Kieee -stdpar=gpu -gpu=$(GPU_ARCH) -mp=gpu
#   DATA=acc            : -O2 -Kieee -stdpar=gpu -gpu=$(GPU_ARCH) -acc
#   DATA=none (host MC) : -O2 -Kieee -stdpar=multicore

# ===========================================================================
# Cray / CCE (ftn) — DC offload + OpenACC data mapping  [add your flags]
# ===========================================================================
#   FC       = ftn
#   FC_FLAGS = -O3 <cray DC-offload + OpenACC flags here>
#   MODFLAG  = -J build
#   DATA     = acc

# ===========================================================================
# AMD / amdflang — DC offloads to AMD GPU via OpenMP do-concurrent mapping
# ===========================================================================
#   FC       = amdflang
#   FC_FLAGS = -O2 -fopenmp --offload-arch=gfx90a -fdo-concurrent-to-openmp=device
#   MODFLAG  = -J build

# ===========================================================================
# Intel / ifx — GPU offload via OpenMP SPIR64 target
# ===========================================================================
#   FC       = ifx
#   FC_FLAGS = -O3 -qopenmp -fopenmp-targets=spir64 -fopenmp-target-do-concurrent
#   MODFLAG  = -module build

# ===========================================================================
# gfortran / ifx — host serial reference (DC runs on the CPU)
# ===========================================================================
#   gfortran:  FC = gfortran ; FC_FLAGS = -O2 ; MODFLAG = -J build   ; DATA = none
#   ifx:       FC = ifx      ; FC_FLAGS = -O2 ; MODFLAG = -module build ; DATA = none
