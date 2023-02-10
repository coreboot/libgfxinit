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

with HW.GFX.GMA.Config_Helpers;

private package HW.GFX.GMA.Power_And_Clocks is

   subtype Refclk_Range is Frequency_Type range 19_200_000 .. 38_400_000;
   subtype Refclk_Range_KHz is Pos64
      range (Refclk_Range'First / 1000) .. (Refclk_Range'Last / 1000);

   procedure Pre_All_Off;
   procedure Post_All_Off;

   procedure Initialize;

   procedure Limit_Dotclocks
     (Configs           : in out Pipe_Configs;
      CDClk_Switch      : out Boolean)
   with
      Post => Config_Helpers.Stable_FB (Configs'Old, Configs);

   procedure Update_CDClk (Configs : in out Pipe_Configs)
   with
      Post => Config_Helpers.Stable_FB (Configs'Old, Configs);
   procedure Enable_CDClk;

   procedure Power_Set_To (Configs : Pipe_Configs);
   procedure Power_Up (Old_Configs, New_Configs : Pipe_Configs);
   procedure Power_Down (Old_Configs, Tmp_Configs, New_Configs : Pipe_Configs);

   procedure Power_Up_Aux;

   procedure Get_Refclk (Refclk : out Refclk_Range);

end HW.GFX.GMA.Power_And_Clocks;
