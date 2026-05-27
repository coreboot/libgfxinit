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

   type Power_Domain is
     (PW1, PW2, PW3, PW4, PW5,
      DDI_A, DDI_B, DDI_C,
      DDI_USBC1, DDI_USBC2, DDI_USBC3, DDI_USBC4, DDI_USBC5, DDI_USBC6,
      AUX_A, AUX_B, AUX_C,
      AUX_USBC1, AUX_USBC2, AUX_USBC3, AUX_USBC4, AUX_USBC5, AUX_USBC6);
   subtype Dynamic_Domain  is Power_Domain range PW2 .. Power_Domain'Last;
   subtype PW_Domain       is Power_Domain range PW1 .. PW5;
   subtype Dynamic_Well    is Power_Domain range PW2 .. PW_Domain'Last;
   subtype Port_Domain     is Power_Domain range DDI_A .. AUX_USBC6;
   subtype DDI_Domain      is Power_Domain range DDI_A .. DDI_USBC6;
   subtype DDI_USBC_Domain is Power_Domain range DDI_USBC1 .. DDI_USBC6;
   subtype AUX_Domain      is Power_Domain range AUX_A .. AUX_USBC6;
   subtype AUX_USBC_Domain is Power_Domain range AUX_USBC1 .. AUX_USBC6;

   ----------------------------------------------------------------------------

   FUSE_STATUS_PGx_DIST_STATUS : constant array (PW_Domain) of Word32 :=
     (PW1   => 1 * 2 ** 26,
      PW2   => 1 * 2 ** 25,
      PW3   => 1 * 2 ** 24,
      PW4   => 1 * 2 ** 23,
      PW5   => 1 * 2 ** 22);

   ----------------------------------------------------------------------------

   function PW_State_Mask (PW : PW_Domain) return Word32
   is
     (1 * 2 ** (2 * (PW_Domain'Pos (PW) - PW_Domain'Pos (PW_Domain'First))));

   ----------------------------------------------------------------------------

   function To_GPU_Port (PD : Port_Domain) return GPU_Port
   is
     (case PD is
         when DDI_A | AUX_A         => DIGI_A,
         when DDI_B | AUX_B         => DIGI_B,
         when DDI_C | AUX_C         => DIGI_C,
         when DDI_USBC1 | AUX_USBC1 => DDI_TC1,
         when DDI_USBC2 | AUX_USBC2 => DDI_TC2,
         when DDI_USBC3 | AUX_USBC3 => DDI_TC3,
         when DDI_USBC4 | AUX_USBC4 => DDI_TC4,
         when DDI_USBC5 | AUX_USBC5 => DDI_TC5,
         when DDI_USBC6 | AUX_USBC6 => DDI_TC6);

   ----------------------------------------------------------------------------

   function Need_PW (PW : Dynamic_Well; Configs : Pipe_Configs) return Boolean;

   procedure Power_Up (Port : Active_Port_Type; Success : out Boolean);

end HW.GFX.GMA.Power_Domains;
