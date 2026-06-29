# dc_patterns/make/common.mk — generic build rules for one mini-app.
#
# A pattern/model Makefile sets, then includes this file:
#   EXE      := <executable name>
#   SRCS     := <its own .F90 TUs, in dependency order>
#   DCP_ROOT ?= ../..
#   include $(DCP_ROOT)/make/common.mk
#
# Separate compilation (one .o per TU) is deliberate: it reproduces the real
# library's translation-unit boundaries, where NVHPC's cross-module device
# codegen quirks actually live. Serial build (.NOTPARALLEL) so the listed
# dependency order of SRCS is honoured without per-file module deps.

DCP_ROOT ?= ../..
include $(DCP_ROOT)/config.mk

# DATA -> cpp define consumed by common/dcp_directives.h
DATA_DEF_omp  := -DDCP_DATA_OMP
DATA_DEF_acc  := -DDCP_DATA_ACC
DATA_DEF_none :=
DATA_DEF := $(DATA_DEF_$(DATA))

CPPFLAGS := $(DATA_DEF) -I$(DCP_ROOT)/common -Ibuild

COMMON_SRCS := $(DCP_ROOT)/common/dcp_kinds.F90 $(DCP_ROOT)/common/dcp_harness.F90
ALL_SRCS := $(COMMON_SRCS) $(SRCS)
OBJS := $(addprefix build/,$(notdir $(ALL_SRCS:.F90=.o)))

BIN := build/$(EXE)

vpath %.F90 $(sort $(dir $(ALL_SRCS)))

.NOTPARALLEL:
.PHONY: all run clean
all: $(BIN)

build:
	@mkdir -p build

build/%.o: %.F90 | build
	$(FC) $(FC_FLAGS) $(CPPFLAGS) $(MODFLAG) -c $< -o $@

$(BIN): $(OBJS)
	$(FC) $(FC_FLAGS) $(OBJS) -o $@

run: $(BIN)
	./$(BIN) $(ARGS)

clean:
	@rm -rf build
