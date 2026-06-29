#!/usr/bin/env bash
# Build + run every dc_patterns app (patterns/* and models/*) under each
# toolchain preset, and print a PASS/FAIL + Mcells/s table — ONE ROW PER
# RESULT line, so apps that emit several variants (e.g. p02_explicit /
# p02_assumed, p03_kji / p03_ijk) get a row each.
#
# Toolchain presets: "label|FC|FC_FLAGS|MODFLAG|DATA".  Edit the array below
# for your machine, or point TC_FILE at a file with one preset per line.
set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ARGS="${ARGS:---n 4000000 --iters 50}"

if [[ -n "${TC_FILE:-}" ]]; then
  mapfile -t TC < "$TC_FILE"
else
  TC=(
    "gfortran-host|gfortran|-O2|-J build|none"
    "nvhpc-omp|nvfortran|-O2 -Kieee -stdpar=gpu -gpu=cc70 -mp=gpu|-module build|omp"
    "nvhpc-acc|nvfortran|-O2 -Kieee -stdpar=gpu -gpu=cc70 -acc|-module build|acc"
  )
fi
NTC=${#TC[@]}

APPS=()
for d in "$ROOT"/patterns/*/ "$ROOT"/models/*/; do
  [[ -f "$d/Makefile" ]] && APPS+=("$d")
done

NW=28; CW=20

printf "%-${NW}s" "result"
for t in "${TC[@]}"; do printf "%-${CW}s" "${t%%|*}"; done
printf '\n'

for app in "${APPS[@]}"; do
  declare -A cell=()
  names=()
  for ti in $(seq 0 $((NTC - 1))); do
    IFS='|' read -r label fc flags modflag data <<< "${TC[$ti]}"
    out=$(make -C "$app" clean >/dev/null 2>&1; \
          make -C "$app" FC="$fc" FC_FLAGS="$flags" MODFLAG="$modflag" DATA="$data" \
               ARGS="$ARGS" run 2>/dev/null)
    while IFS= read -r line; do
      [[ "$line" == RESULT* ]] || continue
      name=$(awk '{print $2}' <<< "$line")
      st=$(sed -n 's/.* status=\([A-Z]*\).*/\1/p' <<< "$line")
      mc=$(sed -n 's/.* mcells_per_s=\([0-9.Ee+-]*\).*/\1/p' <<< "$line")
      if [[ "$st" == PASS ]]; then
        cell["$name|$ti"]="PASS $(printf '%.0f' "$mc" 2>/dev/null)"
      else
        cell["$name|$ti"]="${st:-FAIL}"
      fi
      local_seen=0
      for n in "${names[@]}"; do [[ "$n" == "$name" ]] && local_seen=1 && break; done
      [[ $local_seen -eq 0 ]] && names+=("$name")
    done <<< "$out"
  done
  if [[ ${#names[@]} -eq 0 ]]; then
    printf "%-${NW}s" "$(basename "$app")"
    for ti in $(seq 0 $((NTC - 1))); do printf "%-${CW}s" "BUILD-FAIL"; done
    printf '\n'
  else
    for name in "${names[@]}"; do
      printf "%-${NW}s" "$name"
      for ti in $(seq 0 $((NTC - 1))); do
        printf "%-${CW}s" "${cell[$name|$ti]:-BUILD-FAIL}"
      done
      printf '\n'
    done
  fi
  unset cell
done
