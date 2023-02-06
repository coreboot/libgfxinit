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

with HW.GFX.GMA.DP_Info;

private package HW.GFX.GMA.Connectors.Combo_Phy is

   procedure Set_Signal_Levels
     (Port        : Combo_Port;
      eDP         : Boolean;
      Link        : DP_Link;
      Train_Set   : DP_Info.Train_Set);

   procedure Enable_HDMI (Port : Combo_Port);

end HW.GFX.GMA.Connectors.Combo_Phy;
