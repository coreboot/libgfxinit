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

with HW.GFX.GMA.DP_Aux_Ch;
with HW.GFX.DP_Training;
with HW.GFX.GMA.Combo_Phy;
with HW.GFX.GMA.Config;
with HW.GFX.GMA.Config_Helpers;
with HW.GFX.GMA.Connectors.TC;
with HW.GFX.GMA.Connectors.Combo_Phy;
with HW.GFX.GMA.DP_Aux_Request;
with HW.GFX.GMA.DP_Info;
with HW.GFX.GMA.Panel;
with HW.GFX.GMA.Power_And_Clocks;
with HW.GFX.GMA.Registers;
with HW.GFX.GMA.Transcoder;

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Connectors is

   type Pipe_Regs is array (Pipe_Index) of Registers.Registers_Index;
   DP_TP_CTL : constant Pipe_Regs :=
     (Primary   => Registers.TGL_DP_TP_CTL_A,
      Secondary => Registers.TGL_DP_TP_CTL_B,
      Tertiary  => Registers.TGL_DP_TP_CTL_C);
   DP_TP_CTL_TRANSPORT_ENABLE       : constant := 1 * 2 ** 31;
   DP_TP_CTL_MODE_SST               : constant := 0 * 2 ** 27;
   DP_TP_CTL_MODE_MST               : constant := 1 * 2 ** 27;
   DP_TP_CTL_FORCE_ACT              : constant := 1 * 2 ** 25;
   DP_TP_CTL_ENHANCED_FRAME_ENABLE  : constant := 1 * 2 ** 18;
   DP_TP_CTL_LINK_TRAIN_MASK        : constant := 7 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_PAT1        : constant := 0 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_PAT2        : constant := 1 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_IDLE        : constant := 2 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_NORMAL      : constant := 3 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_PAT3        : constant := 4 * 2 **  8;
   DP_TP_CTL_LINK_TRAIN_PAT4        : constant := 5 * 2 **  8;

   DP_TP_STATUS : constant Pipe_Regs :=
     (Primary   => Registers.TGL_DP_TP_STATUS_A,
      Secondary => Registers.TGL_DP_TP_STATUS_B,
      Tertiary  => Registers.TGL_DP_TP_STATUS_C);
   DP_TP_STATUS_MIN_IDLES_SENT      : constant := 1 * 2 ** 25;

   DDI_BUF_CTL_BUFFER_ENABLE        : constant :=      1 * 2 ** 31;
   DDI_BUF_CTL_PORT_WIDTH_MASK      : constant :=      7 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_1_LANE    : constant :=      0 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_2_LANES   : constant :=      1 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_4_LANES   : constant :=      3 * 2 **  1;
   DDI_BUF_CTL_PORT_REVERSAL        : constant :=      1 * 2 ** 16;
   DDI_BUF_CTL_IDLE_STATUS          : constant :=      1 * 2 **  7;
   DDI_BUF_CTL_TRANS_SELECT_MASK    : constant := 16#f#  * 2 ** 24;

   DDI_CLK_SEL_SHIFT : constant array (Combo_Port) of Natural :=
     (DIGI_A => 0,
      DIGI_B => 2,
      DIGI_C => 4);

   function DDI_CLK_OFF (Port : TGL_Digital_Port) return Word32
   is
     (case Port is
         when DIGI_A  => 1 * 2 ** 10,
         when DIGI_B  => 1 * 2 ** 11,
         when DIGI_C  => 1 * 2 ** 24,
         when DDI_TC1 => 1 * 2 ** 12,
         when DDI_TC2 => 1 * 2 ** 13,
         when DDI_TC3 => 1 * 2 ** 14,
         when DDI_TC4 => 1 * 2 ** 21,
         when DDI_TC5 => 1 * 2 ** 22,
         when DDI_TC6 => 1 * 2 ** 23,
         when others  => 0);

   function DDI_BUF_CTL (Port : TGL_Digital_Port) return Registers.Registers_Index
   is
     (case Port is
         when DIGI_A  => Registers.DDI_BUF_CTL_A,
         when DIGI_B  => Registers.DDI_BUF_CTL_B,
         when DIGI_C  => Registers.DDI_BUF_CTL_C,
         when DDI_TC1 => Registers.DDI_BUF_CTL_USBC1,
         when DDI_TC2 => Registers.DDI_BUF_CTL_USBC2,
         when DDI_TC3 => Registers.DDI_BUF_CTL_USBC3,
         when DDI_TC4 => Registers.DDI_BUF_CTL_USBC4,
         when DDI_TC5 => Registers.DDI_BUF_CTL_USBC5,
         when DDI_TC6 => Registers.DDI_BUF_CTL_USBC6,
         when others  => Registers.DDI_BUF_CTL_A);

   DDI_CLK_SEL : constant array (USBC_Port) of Registers.Registers_Index :=
     (DDI_TC1 => Registers.DDI_CLK_SEL_USBC1,
      DDI_TC2 => Registers.DDI_CLK_SEL_USBC2,
      DDI_TC3 => Registers.DDI_CLK_SEL_USBC3,
      DDI_TC4 => Registers.DDI_CLK_SEL_USBC4,
      DDI_TC5 => Registers.DDI_CLK_SEL_USBC5,
      DDI_TC6 => Registers.DDI_CLK_SEL_USBC6);

   type Training_Port_Info is record
      Port  : TGL_Digital_Port;
      Pipe  : Pipe_Index;
      eDP   : Boolean;
   end record;

   function To_DP (Port_Info : Training_Port_Info) return DP_Port is
     (case Port_Info.Port is
      when DIGI_A  => DP_A,
      when DIGI_B  => DP_B,
      when DIGI_C  => DP_C,
      when DDI_TC1 => DP_D,
      when DDI_TC2 => DP_E,
      when DDI_TC3 => DP_F,
      when DDI_TC4 => DP_G,
      when DDI_TC5 => DP_H,
      when DDI_TC6 => DP_I,
      when others  => DP_A);

   ---------------------------------------------------------------------

   procedure Off (Pipe : Pipe_Index; Port : TGL_Digital_Port)
   is
      Enabled : Boolean;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      Registers.Is_Set_Mask
        (Register => DDI_BUF_CTL (Port),
         Mask     => DDI_BUF_CTL_BUFFER_ENABLE,
         Result   => Enabled);

      if Enabled then
         Registers.Unset_Mask
           (Register => DDI_BUF_CTL (Port),
            Mask     => DDI_BUF_CTL_BUFFER_ENABLE);
      end if;

      Registers.Unset_Mask
        (Register   => DP_TP_CTL (Pipe),
         Mask       => DP_TP_CTL_TRANSPORT_ENABLE);

      if Enabled then
         Registers.Wait_Set_Mask
           (Register => DDI_BUF_CTL (Port),
            Mask     => DDI_BUF_CTL_IDLE_STATUS);
      end if;

     Registers.Set_Mask
       (Register => Registers.DPCLKA_CFGCR0,
        Mask     => DDI_CLK_OFF (Port));
   end Off;

   procedure Off (Port_Info : Training_Port_Info) is
   begin
      Off (Port_Info.Pipe, Port_Info.Port);
   end Off;

   ---------------------------------------------------------------------

   procedure Set_TP_CTL
     (Pipe : Pipe_Index;
      Link : DP_Link;
      Pattern : DP_Info.Training_Pattern)
   is
      type DP_TP_CTL_LINK_TRAIN_ARRAY is
         array (DP_Info.Training_Pattern) of Word32;
      DP_TP_CTL_LINK_TRAIN : constant DP_TP_CTL_LINK_TRAIN_ARRAY :=
         DP_TP_CTL_LINK_TRAIN_ARRAY'
        (DP_Info.TP_1    => DP_TP_CTL_LINK_TRAIN_PAT1,
         DP_Info.TP_2    => DP_TP_CTL_LINK_TRAIN_PAT2,
         DP_Info.TP_3    => DP_TP_CTL_LINK_TRAIN_PAT3,
         DP_Info.TP_Idle => DP_TP_CTL_LINK_TRAIN_IDLE,
         DP_Info.TP_None => DP_TP_CTL_LINK_TRAIN_NORMAL);

      DP_TP_CTL_Enhanced_Frame : Word32 := 0;
   begin
      if Link.Enhanced_Framing then
         DP_TP_CTL_Enhanced_Frame := DP_TP_CTL_ENHANCED_FRAME_ENABLE;
      end if;

      Registers.Write
        (Register => DP_TP_CTL (Pipe),
         Value    => DP_TP_CTL_TRANSPORT_ENABLE or
                     DP_TP_CTL_Enhanced_Frame or
                     DP_TP_CTL_MODE_SST or
                     DP_TP_CTL_LINK_TRAIN (Pattern));
      Registers.Posting_Read (DP_TP_CTL (Pipe));
   end Set_TP_CTL;

   procedure Set_Training_Pattern
     (Port_Info   : Training_Port_Info;
      Link        : DP_Link;
      Pattern     : DP_Info.Training_Pattern)
   is
      use type DP_Info.Training_Pattern;
      Pipe : Pipe_Index renames Port_Info.Pipe;
   begin
      if Pattern < DP_Info.TP_Idle then
         Set_TP_CTL (Pipe, Link, Pattern);
      else
         Set_TP_CTL (Pipe, Link, DP_Info.TP_Idle);
         Registers.Wait_Set_Mask
           (Register => DP_TP_STATUS (Pipe),
            Mask => DP_TP_STATUS_MIN_IDLES_SENT);
         Set_TP_CTL (Pipe, Link, DP_Info.TP_None);
      end if;
   end Set_Training_Pattern;

   ---------------------------------------------------------------------

   pragma Warnings (GNATprove, Off, "unused variable ""Port_Info""",
                    Reason => "Needed for a common interface");
   function Max_V_Swing
     (Port_Info : Training_Port_Info) return DP_Info.DP_Voltage_Swing
   is
     (DP_Info.VS_Level_3);

   function Max_Pre_Emph
     (Port_Info   : Training_Port_Info;
      Train_Set   : DP_Info.Train_Set)
      return DP_Info.DP_Pre_Emph
   is
   begin
      return
        (case Train_Set.Voltage_Swing is
         when DP_Info.VS_Level_0 => DP_Info.Emph_Level_3,
         when DP_Info.VS_Level_1 => DP_Info.Emph_Level_2,
         when DP_Info.VS_Level_2 => DP_Info.Emph_Level_1,
         when others             => DP_Info.Emph_Level_0);
   end Max_Pre_Emph;
   pragma Warnings (GNATprove, On, "unused variable ""Port_Info""");

   ---------------------------------------------------------------------

   procedure Set_Signal_Levels
     (Port_Info   : Training_Port_Info;
      Link        : DP_Link;
      Train_Set   : DP_Info.Train_Set)
   is
      Port : TGL_Digital_Port renames Port_Info.Port;
   begin
      if Port in Combo_Port then
         Combo_Phy.Set_Signal_Levels (Port, Port_Info.eDP, Link, Train_Set);
      elsif Port in USBC_Port then
         TC.Set_Signal_Levels (Port, Link, Train_Set);
      end if;
   end Set_Signal_Levels;

   ---------------------------------------------------------------------

   procedure Map_PLL_To_Port
     (Port     : TGL_Digital_Port;
      PLL_Hint : Word32)
   is
      CLOCK_SELECT_MASK  : constant := 16#f000_0000#;
      CLOCK_SELECT_TYPEC : constant := 16#8000_0000#;
      DDI_CLK_SEL_MASK : constant := 3;
      Clk_Sel_Shift : Natural;
   begin
      if Port in USBC_Port then
         Registers.Unset_And_Set_Mask
           (Register   => DDI_CLK_SEL (Port),
            Mask_Unset => CLOCK_SELECT_MASK,
            Mask_Set   => CLOCK_SELECT_TYPEC);
      end if;

      if Port in Combo_Port then
         Clk_Sel_Shift := DDI_CLK_SEL_SHIFT (Port);
         Registers.Unset_And_Set_Mask
           (Register   => Registers.DPCLKA_CFGCR0,
            Mask_Unset => Shift_Left (DDI_CLK_SEL_MASK, Clk_Sel_Shift),
            Mask_Set   => Shift_Left (PLL_Hint, Clk_Sel_Shift));
         Registers.Posting_Read (Registers.DPCLKA_CFGCR0);
      end if;
   end Map_PLL_To_Port;

   ---------------------------------------------------------------------

   pragma Warnings (GNAT, Off, """Port_Cfg"" is not modified",
                    Reason => "Needed for a common interface");
   procedure Prepare
     (Port     : in     Active_Port_Type;
      Port_Cfg : in out Port_Config;
      Success  :    out Boolean)
   is
   begin
      if Port_Cfg.Port in USBC_Port then
         TC.Connect
           (Port     => Port_Cfg.Port,
            DP_Alt   => Port in Physical_USBC_Ports,
            Lanes    => Port_Cfg.DP.Lane_Count,
            Success  => Success);
      else
         Success := True;
      end if;
   end Prepare;
   pragma Warnings (GNAT, On, """Port_Cfg"" is not modified");

   ---------------------------------------------------------------------

   procedure Pre_On
     (Pipe        : in     Pipe_Index;
      Port_Cfg    : in     Port_Config;
      PLL_Hint    : in     Word32;
      Success     :    out Boolean)
   is
      package Training is new DP_Training
        (TPS3_Supported    => True,
         T                 => Training_Port_Info,
         Aux_T             => DP_Port,
         Aux_Ch            => HW.GFX.GMA.DP_Aux_Ch,
         DP_Info           => DP_Info,
         To_Aux            => To_DP,
         Max_V_Swing       => Max_V_Swing,
         Max_Pre_Emph      => Max_Pre_Emph,
         Set_Pattern       => Set_Training_Pattern,
         Set_Signal_Levels => Set_Signal_Levels,
         Off               => Off);
      Port : TGL_Digital_Port;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Port_Cfg.Port not in TGL_Digital_Port then
         Success := False;
         return;
      end if;

      Port := Port_Cfg.Port;
      Map_PLL_To_Port (Port, PLL_Hint);

      Registers.Unset_Mask
        (Register => Registers.DPCLKA_CFGCR0,
         Mask     => DDI_CLK_OFF (Port));

      if Port_Cfg.Display = DP then
         if Port in USBC_Port then
            TC.Program_DP_Mode
               (Port, Natural (Lane_Count_As_Integer (Port_Cfg.DP.Lane_Count)));
         end if;
         Transcoder.Enable_Pipe_Clock (Pipe, Port_Cfg);
         Transcoder.Configure (Pipe, Port_Cfg, Scale => False);

         Training.Train_DP
           (Port    => (Port, Pipe, eDP => Port_Cfg.Is_eDP),
            Link    => Port_Cfg.DP,
            Success => Success);
      elsif Port_Cfg.Display = HDMI then
         Transcoder.Enable_Pipe_Clock (Pipe, Port_Cfg);
         Transcoder.Configure (Pipe, Port_Cfg, Scale => False);

         Success := True;
      else
         Success := False;
      end if;
   end Pre_On;

   procedure Post_On
     (Pipe     : in     Pipe_Index;
      Port_Cfg : in     Port_Config;
      PLL_Hint : in     Word32;
      Success  :    out Boolean)
   is
      Port : GPU_Port renames Port_Cfg.Port;
   begin
      Success := True;

      if Port_Cfg.Display = HDMI then
         if Port in Combo_Port then
            Combo_Phy.Enable_HDMI (Port);
         elsif Port in USBC_Port then
            TC.Enable_HDMI (Port);
         end if;
      end if;

      Panel.Backlight_On (Port_Cfg.Panel);
   end Post_On;

   ---------------------------------------------------------------------

   procedure Pre_Off (Pipe : Pipe_Index; Port_Cfg : Port_Config) is
   begin
      Panel.Backlight_Off (Port_Cfg.Panel);
      Panel.Off (Port_Cfg.Panel);
   end Pre_Off;

   procedure Post_Off (Pipe : Pipe_Index; Port_Cfg : Port_Config) is
   begin
      if Port_Cfg.Port in Combo_Port then
         Off (Pipe, Port_Cfg.Port);
      end if;
   end Post_Off;

   procedure Pre_All_Off  is
   begin
      for P in Valid_Panels loop
         Panel.Backlight_Off (P);
         Panel.Off (P);
      end loop;
   end Pre_All_Off;

   procedure Post_All_Off is
   begin
      for Port in Combo_Port loop
         Off (Pipe_Index'First, Port); -- pipe index is arbitrary
      end loop;
   end Post_All_Off;

   procedure Post_Reset_Off is
   begin
      for Port in Combo_Port loop
         Off (Pipe_Index'First, Port);
      end loop;
   end Post_Reset_Off;

   procedure Initialize is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      GMA.Combo_Phy.Initialize;
   end Initialize;

end HW.GFX.GMA.Connectors;
