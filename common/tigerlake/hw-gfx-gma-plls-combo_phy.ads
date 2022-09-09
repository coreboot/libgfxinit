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

with HW.Debug;
with GNAT.Source_Info;

private package HW.GFX.GMA.PLLs.Combo_Phy is

   procedure On
     (PLL      : in     Combo_DPLLs;
      Port_Cfg : in     Port_Config;
      Success  :    out Boolean);

   procedure Free (PLL : Combo_DPLLs);

   procedure All_Off;

   type Value_Array is array (Combo_DPLLs) of Word32;
   Register_Value : constant Value_Array := Value_Array'
     (DPLL0 => 0, DPLL1 => 1);

end HW.GFX.GMA.PLLs.Combo_Phy;
