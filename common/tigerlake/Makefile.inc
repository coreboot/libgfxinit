gfxinit-y += hw-gfx-gma-combo_phy.adb
gfxinit-y += hw-gfx-gma-combo_phy.ads
gfxinit-y += hw-gfx-gma-power_and_clocks.adb
gfxinit-y += hw-gfx-gma-power_and_clocks.ads
gfxinit-y += hw-gfx-gma-connectors.adb
gfxinit-y += hw-gfx-gma-connectors-tc.adb
gfxinit-y += hw-gfx-gma-connectors-tc.ads
gfxinit-y += hw-gfx-gma-connectors-tc-ownership.ads
gfxinit-y += hw-gfx-gma-connectors-combo_phy.adb
gfxinit-y += hw-gfx-gma-connectors-combo_phy.ads
gfxinit-y += hw-gfx-gma-port_detect.adb
gfxinit-y += hw-gfx-gma-plls.adb
gfxinit-y += hw-gfx-gma-plls.ads
gfxinit-y += hw-gfx-gma-plls-combo_phy.adb
gfxinit-y += hw-gfx-gma-plls-combo_phy.ads
gfxinit-y += hw-gfx-gma-plls-dekel_phy.adb
gfxinit-y += hw-gfx-gma-plls-dekel_phy.ads

ifneq ($(filter Tigerlake,$(CONFIG_GFX_GMA_GENERATION)),)
gfxinit-y += tgl/hw-gfx-gma-connectors-tc-ownership.adb
else ifneq ($(filter AlderlakeP,$(CONFIG_GFX_GMA_GENERATION)),)
gfxinit-y += xelpd/hw-gfx-gma-connectors-tc-ownership.adb
endif
