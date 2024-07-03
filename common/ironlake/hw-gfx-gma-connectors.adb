--
-- Copyright (C) 2015-2016 secunet Security Networks AG
-- Copyright (C) 2016 Nico Huber <nico.h@gmx.de>
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
with HW.GFX.GMA.DP_Info;
with HW.GFX.GMA.Registers;
with HW.GFX.GMA.Panel;
with HW.GFX.GMA.Connectors.EDP;
with HW.GFX.GMA.Connectors.FDI;
with HW.GFX.GMA.PCH.VGA;
with HW.GFX.GMA.PCH.LVDS;
with HW.GFX.GMA.PCH.HDMI;
with HW.GFX.GMA.PCH.DP;
with HW.GFX.GMA.PCH.Transcoder;

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Connectors
is

   procedure Post_Reset_Off is null;
   procedure Initialize is null;

   ----------------------------------------------------------------------------

   procedure Prepare
     (Port     : in     Active_Port_Type;
      Port_Cfg : in out Port_Config;
      Success  :    out Boolean)
   is
      -- Override lane count for FDI_B if FDI_C is used
      procedure Override_FDI_Link
      with
         Post => Port_Cfg.Mode = Port_Cfg'Old.Mode
      is
         FDI_TX_CTL_FDI_TX_ENABLE : constant := 1 * 2 ** 31;
         Enabled : Boolean;
      begin
         if Config.Has_FDI_C and then Port_Cfg.Port = DIGI_C then
            -- if DIGI_D enabled: (FDI names are off by one)
            Registers.Is_Set_Mask
              (Register => Registers.FDI_TX_CTL_C,
               Mask     => FDI_TX_CTL_FDI_TX_ENABLE,
               Result   => Enabled);
            if Enabled then
               Port_Cfg.FDI.Receiver_Caps.Max_Lane_Count := DP_Lane_Count_2;
            end if;
         end if;

         DP_Info.Preferred_Link_Setting (Port_Cfg.FDI, Port_Cfg.Mode, Success);
      end Override_FDI_Link;
   begin
      if Port_Cfg.Is_FDI then
         Override_FDI_Link;
      else
         Success := True;
      end if;
   end Prepare;

   ----------------------------------------------------------------------------

   function Is_Internal (Port_Cfg : Port_Config) return Boolean
   is
   begin
      return
         Port_Cfg.Port = DIGI_A or
         (Port_Cfg.Is_FDI and Port_Cfg.PCH_Port = PCH_LVDS);
   end Is_Internal;

   ----------------------------------------------------------------------------

   procedure Pre_On
     (Pipe        : in     Pipe_Index;
      Port_Cfg    : in     Port_Config;
      PLL_Hint    : in     Word32;
      Success     :    out Boolean)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Port_Cfg.Port = DIGI_A then
         EDP.Pre_On (Pipe, Port_Cfg);
      elsif Port_Cfg.Port in FDI.GPU_FDI_Port then
         FDI.Pre_On (Port_Cfg);
      end if;
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

      if Port_Cfg.Port = DIGI_A then
         EDP.Pre_Training;
         Panel.On (Port_Cfg.Panel, Wait => True);
         EDP.Post_On (Port_Cfg.DP, Success);
      elsif Port_Cfg.Port in FDI.GPU_FDI_Port then
         declare
            FDI_Port : constant PCH.FDI_Port_Type :=
               FDI.PCH_FDIs (Port_Cfg.Port);
         begin
            FDI.Post_On (Port_Cfg, Success);

            if Success then
               PCH.Transcoder.On (Port_Cfg, FDI_Port, PLL_Hint);
               if Port_Cfg.PCH_Port = PCH_DAC then
                  PCH.VGA.On (FDI_Port, Port_Cfg.Mode);
               elsif Port_Cfg.PCH_Port = PCH_LVDS then
                  PCH.LVDS.On (Port_Cfg, FDI_Port);
               elsif Port_Cfg.PCH_Port in PCH.HDMI.IRL_PCH_HDMI_Port then
                  PCH.HDMI.On (Port_Cfg, FDI_Port);
               elsif Port_Cfg.PCH_Port in PCH_DP_Port then
                  PCH.DP.On (Port_Cfg, FDI_Port, Success);
               end if;
            end if;
         end;
      else
         Success := False;
      end if;

      if Success and Is_Internal (Port_Cfg) then
         Panel.On (Port_Cfg.Panel, Wait => False);
         Panel.Backlight_On (Port_Cfg.Panel);
      end if;
   end Post_On;

   ----------------------------------------------------------------------------

   procedure Pre_Off (Pipe : Pipe_Index; Port_Cfg : Port_Config)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Is_Internal (Port_Cfg) then
         Panel.Backlight_Off (Port_Cfg.Panel);
         Panel.Off (Port_Cfg.Panel);
      end if;
   end Pre_Off;

   procedure Post_Off (Pipe : Pipe_Index; Port_Cfg : Port_Config)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Port_Cfg.Port = DIGI_A then
         EDP.Off (Port_Cfg.Port);
      elsif Port_Cfg.Port in FDI.GPU_FDI_Port then
         declare
            FDI_Port : constant PCH.FDI_Port_Type :=
               FDI.PCH_FDIs (Port_Cfg.Port);
         begin
            if Port_Cfg.PCH_Port in PCH_DP_Port then
               PCH.DP.Off (Port_Cfg.PCH_Port);
            end if;

            FDI.Off (Port_Cfg.Port, FDI.Link_Off);

            if Port_Cfg.PCH_Port = PCH_DAC then
               PCH.VGA.Off;
            elsif Port_Cfg.PCH_Port = PCH_LVDS then
               PCH.LVDS.Off;
            elsif Port_Cfg.PCH_Port in PCH.HDMI.IRL_PCH_HDMI_Port then
               PCH.HDMI.Off (Port_Cfg.PCH_Port);
            end if;
            PCH.Transcoder.Off (FDI_Port);

            FDI.Off (Port_Cfg.Port, FDI.Clock_Off);
         end;
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

      EDP.Off (DIGI_A);

      for Port in FDI.GPU_FDI_Port loop
         FDI.Off (Port, FDI.Link_Off);
      end loop;
      PCH.VGA.Off;
      PCH.LVDS.Off;
      PCH.HDMI.All_Off;
      PCH.DP.All_Off;
      for Port in PCH.FDI_Port_Type loop
         PCH.Transcoder.Off (Port);
      end loop;
      for Port in FDI.GPU_FDI_Port loop
         FDI.Off (Port, FDI.Clock_Off);
      end loop;
   end Post_All_Off;

end HW.GFX.GMA.Connectors;
