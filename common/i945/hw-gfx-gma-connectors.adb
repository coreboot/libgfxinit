--
-- Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>
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

with HW.GFX.GMA.Config;
with HW.GFX.GMA.Panel;
with HW.GFX.GMA.GMCH.VGA;
with HW.GFX.GMA.GMCH.LVDS;

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Connectors
is

   procedure Post_Reset_Off is null;
   procedure Initialize is null;

   ----------------------------------------------------------------------------

   procedure Pre_On
     (Pipe        : in     Pipe_Index;
      Port_Cfg    : in     Port_Config;
      PLL_Hint    : in     Word32;
      Success     :    out Boolean)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      Success := True;
   end Pre_On;

   procedure Post_On
     (Pipe     : in     Pipe_Index;
      Port_Cfg : in     Port_Config;
      PLL_Hint : in     Word32;
      Success  :    out Boolean)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      Success := True;
      if Port_Cfg.Port = LVDS then
         GMCH.LVDS.On (Port_Cfg, Pipe);
      elsif Port_Cfg.Port = VGA then
         GMCH.VGA.On (Pipe, Port_Cfg.Mode);
      end if;

      Panel.On (Port_Cfg.Panel, Wait => False);
      Panel.Backlight_On (Port_Cfg.Panel);
   end Post_On;

   ----------------------------------------------------------------------------

   procedure Pre_Off (Port_Cfg : Port_Config)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Panel.Backlight_Off (Port_Cfg.Panel);
      Panel.Off (Port_Cfg.Panel);
   end Pre_Off;

   procedure Post_Off (Port_Cfg : Port_Config)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      if Port_Cfg.Port = LVDS then
         GMCH.LVDS.Off;
      elsif Port_Cfg.Port = VGA then
         GMCH.VGA.Off;
      end if;
   end Post_Off;

   ----------------------------------------------------------------------------

   procedure Pre_All_Off
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for P in Valid_Panels loop
         Panel.Backlight_Off (P);
         Panel.Off (P);
      end loop;
   end Pre_All_Off;

   procedure Post_All_Off
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      GMCH.LVDS.Off;
      GMCH.VGA.Off;
   end Post_All_Off;

end HW.GFX.GMA.Connectors;
