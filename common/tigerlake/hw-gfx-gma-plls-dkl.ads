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

private package HW.GFX.GMA.PLLs.DKL is

   procedure On
     (PLL      : in     DKL_DPLLs;
      Port_Cfg : in     Port_Config;
      Success  :    out Boolean);

   procedure Free (PLL : DKL_DPLLs);

   procedure All_Off;

end HW.GFX.GMA.PLLs.DKL;
