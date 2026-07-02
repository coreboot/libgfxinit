name := gfxinit

ifeq ($(MAKECMDGOALS),gfx_test)
prefixed-name	:= gfx_test
link-type	:= program
GFXINIT_TEST	:= y
endif

gfxinit-deplibs := libhw

libhw-dir ?= ../libhwbase/dest
include $(libhw-dir)/Makefile

$(gfxinit-objs): ADAFLAGS += -gnaty3abdefhiklnprSx

# Allow statements after then/else as we arrange cmdline parsing as a table
$(call src-to-obj,,gfxtest/hw-gfx-gma-gfx_test.adb): ADAFLAGS += -gnaty-S

gfx_test: $(binary)

.PHONY: gfx_test
