# dc_patterns top-level — fan out to every pattern + model.
# Pass-through: `make TOOLCHAIN-style` config comes from config.mk / CLI vars.
DCP_ROOT := $(CURDIR)
SUBDIRS  := $(wildcard patterns/*/ models/*/)

.PHONY: all run clean
all:
	@for d in $(SUBDIRS); do $(MAKE) -C $$d DCP_ROOT=$(DCP_ROOT) all || exit 1; done

run:
	@for d in $(SUBDIRS); do $(MAKE) -C $$d DCP_ROOT=$(DCP_ROOT) run || exit 1; done

clean:
	@for d in $(SUBDIRS); do $(MAKE) -C $$d DCP_ROOT=$(DCP_ROOT) clean; done
