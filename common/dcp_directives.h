/* dc_patterns/common/dcp_directives.h
 *
 * Backend-toggled data-mapping + device-routine directives. The COMPUTE is
 * always a bare `do concurrent`; only these lines change per compiler.
 * Selected by -DDCP_DATA_{OMP,ACC} (or neither => host). See config.mk.
 *
 * A macro that expands to `!$omp ...` / `!$acc ...` is emitted verbatim onto
 * its own source line; the Fortran sentinel is then honoured when the matching
 * directive flag (-mp / -acc) is on.
 */
#ifndef DCP_DIRECTIVES_H
#define DCP_DIRECTIVES_H

#if defined(DCP_DATA_OMP)
#  define DCP_DATA_NAME "omp"
#  define DCP_DECLARE_TARGET !$omp declare target
#  define DCP_ENTER(x) !$omp target enter data map(to: x)
#  define DCP_CREATE(x) !$omp target enter data map(alloc: x)
#  define DCP_EXIT(x) !$omp target exit data map(delete: x)
#  define DCP_UPDATE_SELF(x) !$omp target update from(x)
/* graph/async wrap is an NVHPC-OpenACC capability; inert here (bare DC) */
#  define DCP_KERNELS_ASYNC
#  define DCP_KERNELS_END
#  define DCP_WAIT
#elif defined(DCP_DATA_ACC)
#  define DCP_DATA_NAME "acc"
#  define DCP_DECLARE_TARGET !$acc routine seq
#  define DCP_ENTER(x) !$acc enter data copyin(x)
#  define DCP_CREATE(x) !$acc enter data create(x)
#  define DCP_EXIT(x) !$acc exit data delete(x)
#  define DCP_UPDATE_SELF(x) !$acc update self(x)
/* wrapping a bare DC in `acc kernels async` makes it CUDA-graph-capturable */
#  define DCP_KERNELS_ASYNC !$acc kernels async(1)
#  define DCP_KERNELS_END !$acc end kernels
#  define DCP_WAIT !$acc wait
#else
#  define DCP_DATA_NAME "host"
#  define DCP_DECLARE_TARGET
#  define DCP_ENTER(x)
#  define DCP_CREATE(x)
#  define DCP_EXIT(x)
#  define DCP_UPDATE_SELF(x)
#  define DCP_KERNELS_ASYNC
#  define DCP_KERNELS_END
#  define DCP_WAIT
#endif

#endif /* DCP_DIRECTIVES_H */
