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

private package HW.GFX.GMA.PLLs
with
   Abstract_State => (State with Part_Of => GMA.State)
is

   -- XXX: Types should be private (but that triggers a bug in SPARK GPL 2016)
   type T is (Invalid_PLL, DPLL0, DPLL1, TCPLL1, TCPLL2, TCPLL3, TCPLL4, TCPLL5, TCPLL6);
   subtype Configurable_DPLLs is T range DPLL0 .. TCPLL6;
   subtype Combo_DPLLs is T range DPLL0 .. DPLL1;
   subtype DKL_DPLLs is T range TCPLL1 .. TCPLL6;
   Invalid : constant T := Invalid_PLL;

   procedure Initialize
   with
      Global => (Output => State);

   procedure Alloc
     (Port_Cfg : in     Port_Config;
      PLL      :    out T;
      Success  :    out Boolean);

   procedure Free (PLL : T);

   procedure All_Off;

   function Register_Value (PLL : T) return Word32;

end HW.GFX.GMA.PLLs;
