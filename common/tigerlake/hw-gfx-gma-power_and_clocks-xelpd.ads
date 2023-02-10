-- (C) 2022 Google, LLC
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

private package HW.GFX.GMA.Power_And_Clocks.XELPD is

   type Power_Domain is
     (PW1, PWA, PW2, PWB, PWC, PWD,
      AUX_A, AUX_B, AUX_USBC1, AUX_USBC2, AUX_USBC3, AUX_USBC4,
      DDI_A, DDI_B, DDI_USBC1, DDI_USBC2, DDI_USBC3, DDI_USBC4);

   procedure Power_Set_To (Configs : Pipe_Configs);
   procedure Power_Up (Old_Configs, New_Configs : Pipe_Configs);
   procedure Power_Down (Old_Configs, Tmp_Configs, New_Configs : Pipe_Configs);
   procedure All_Off;
   function Need_PD (PD : Power_Domain; Configs : Pipe_Configs) return Boolean;
   procedure Init_Power;
   procedure Aux_Off;
   procedure Power_Up_Aux;

end HW.GFX.GMA.Power_And_Clocks.XELPD;
