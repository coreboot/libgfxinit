--
-- Copyright (C) 2022 Google, LLC
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--

private package HW.GFX.GMA.Power_Domains is

   -- Comment lifted from i915:
   -- XE_LPD Power Domains
   --
   -- Previous platforms required that PW(n-1) be enabled before PW(n).  That
   -- dependency chain turns into a dependency tree on XE_LPD:
   --
   --       PW0
   --        |
   --     --PW1--
   --    /       \
   --  PWA     --PW2--
   --         /   |   \
   --       PWB  PWC  PWD
   --
   -- Power wells must be enabled from top to bottom and disabled from bottom
   -- to top.  This allows pipes to be power gated independently.

   type Power_Domain is
     (PW1, PWA, PW2, PWB, PWC, PWD,
      -- order matters, bit positions for D & E replace USBC5 & 6 from TGL:
      AUX_A, AUX_B, AUX_C, AUX_USBC1, AUX_USBC2, AUX_USBC3, AUX_USBC4, AUX_D, AUX_E,
      DDI_A, DDI_B, DDI_C, DDI_USBC1, DDI_USBC2, DDI_USBC3, DDI_USBC4, DDI_D, DDI_E);
   subtype Dynamic_Domain  is Power_Domain range PWA .. DDI_E;
   subtype PW_Domain       is Power_Domain range PW1 .. PWD;
   subtype Dynamic_Well    is Power_Domain range PWA .. PWD;
   subtype Port_Domain     is Power_Domain range AUX_A .. DDI_E;
   subtype DDI_Domain      is Power_Domain range DDI_A .. DDI_E;
   subtype DDI_PW2_Domain  is Power_Domain range DDI_C .. DDI_E;
   subtype DDI_USBC_Domain is Power_Domain range DDI_USBC1 .. DDI_USBC4;
   subtype AUX_Domain      is Power_Domain range AUX_A .. AUX_E;
   subtype AUX_USBC_Domain is Power_Domain range AUX_USBC1 .. AUX_USBC4;

   ----------------------------------------------------------------------------

   FUSE_STATUS_PGx_DIST_STATUS : constant array (PW_Domain) of Word32 :=
     (PW1   => 1 * 2 ** 26,
      PW2   => 1 * 2 ** 25,
      PWA   => 1 * 2 ** 21,
      PWB   => 1 * 2 ** 20,
      PWC   => 1 * 2 ** 19,
      PWD   => 1 * 2 ** 18);

   ----------------------------------------------------------------------------

   PW_State_Mask : constant array (PW_Domain) of Word32 :=
     (PW1 => 1 * 2 **  0,
      PW2 => 1 * 2 **  2,
      PWA => 1 * 2 ** 10,
      PWB => 1 * 2 ** 12,
      PWC => 1 * 2 ** 14,
      PWD => 1 * 2 ** 16);

   ----------------------------------------------------------------------------

   function To_GPU_Port (PD : Port_Domain) return GPU_Port
   is
     (case PD is
         when DDI_A | AUX_A         => DIGI_A,
         when DDI_B | AUX_B         => DIGI_B,
         when DDI_C | AUX_C         => DIGI_C,
         when DDI_D | AUX_D         => DIGI_D,
         when DDI_E | AUX_E         => DIGI_E,
         when DDI_USBC1 | AUX_USBC1 => DDI_TC1,
         when DDI_USBC2 | AUX_USBC2 => DDI_TC2,
         when DDI_USBC3 | AUX_USBC3 => DDI_TC3,
         when DDI_USBC4 | AUX_USBC4 => DDI_TC4);

   ----------------------------------------------------------------------------

   function Need_PW (PW : Dynamic_Well; Configs : Pipe_Configs) return Boolean;

   procedure Power_Up (Port : Active_Port_Type; Success : out Boolean);

end HW.GFX.GMA.Power_Domains;
