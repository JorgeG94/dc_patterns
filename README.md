# dc_patterns

Standalone, dependency-free `do concurrent` portability reproducers. Each
mini-app is built from ≥2 separately-compiled TUs and links nothing but the
Fortran + offload runtime.
for the design.

## Concept

`do concurrent` does the compute in every kernel. Only the **data-mapping**
directive layer changes per compiler, selected by `DATA`:

| `DATA` | layer | for |
|--------|-------|-----|
| `omp`  | `!$omp target enter data` / `declare target` | NVHPC, AMD (portable default) |
| `acc`  | `!$acc enter data` / `routine seq`           | Cray/CCE; NVHPC cross-check |
| `none` | none — bare DC on the host                   | gfortran/ifx serial reference |

## Build & run

Edit [`config.mk`](config.mk) for your machine (`FC`, `FC_FLAGS`, `DATA`,
`MODFLAG`) — the NVIDIA/NVHPC block is pre-filled — or override on the CLI:

```sh
# one pattern, default (NVHPC, omp data):
make -C patterns/p01_acc_routine_xtu run

# NVHPC, OpenACC data layer:
make -C patterns/p01_acc_routine_xtu DATA=acc \
     FC_FLAGS='-O2 -Kieee -stdpar=gpu -gpu=cc70 -acc' run

# gfortran host reference:
make -C patterns/p01_acc_routine_xtu FC=gfortran FC_FLAGS=-O2 \
     MODFLAG='-J build' DATA=none run

# Cray/CCE: fill the ftn block in config.mk, then:
make -C patterns/p01_acc_routine_xtu FC=ftn FC_FLAGS='...' \
     MODFLAG='-J build' DATA=acc run

# everything:
make run            # uses config.mk
```

Each exe prints one line:

```
RESULT <name> data=<omp|acc|host> n=.. iters=.. status=PASS|FAIL max_abs_err=.. mcells_per_s=..
```

PASS ⇒ exit 0; FAIL ⇒ non-zero (so a matrix runner / CI can gate).

## Catalog

- `patterns/` — single-hazard reproducers (p01 = cross-module device routine, …).
- `models/` — representative miniatures of real kernel classes (continuity,
  remap, hor_visc, vert_visc, kappa_shear, barotropic).
