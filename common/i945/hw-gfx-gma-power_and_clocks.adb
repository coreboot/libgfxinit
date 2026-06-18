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
with HW.GFX.GMA.Registers;

package body HW.GFX.GMA.Power_And_Clocks is

   CLKCFG_FSB_MASK   : constant := 7 * 2 ** 0;
   -- hrawclk = FSB / 4. The CLKCFG FSB bits encode different FSB speeds
   -- depending on whether the platform is mobile or desktop (ALT encoding):
   --   mobile:  0=FSB400,  1=FSB533, 2=FSB800, 3=FSB667, 6=FSB1067, 7=FSB1333
   --   desktop: 0=FSB1067, 1=FSB533, 2=FSB800, 3=FSB667, 4=FSB1333,
   --            5=FSB400,  6=FSB1600
   HRAWCLK_100       : constant Frequency_Type := 100_000_000;  -- FSB 400
   HRAWCLK_133       : constant Frequency_Type := 133_333_333;  -- FSB 533
   HRAWCLK_167       : constant Frequency_Type := 166_666_666;  -- FSB 667
   HRAWCLK_200       : constant Frequency_Type := 200_000_000;  -- FSB 800
   HRAWCLK_267       : constant Frequency_Type := 266_666_667;  -- FSB 1067
   HRAWCLK_333       : constant Frequency_Type := 333_333_333;  -- FSB 1333
   HRAWCLK_400       : constant Frequency_Type := 400_000_000;  -- FSB 1600 (desktop)

   -- i945/Pineview CDClk values (from Linux kernel intel_cdclk.c):
   --
   -- i945G (desktop): fixed 400 MHz CDClk
   --
   -- i945GM (mobile): read from GCFGC PCI register (0xF0)
   --   GC_LOW_FREQUENCY_ENABLE (bit 7) => 133 MHz
   --   GC_DISPLAY_CLOCK_190_200_MHZ (0 << 4) => 200 MHz (default)
   --   GC_DISPLAY_CLOCK_333_320_MHZ (4 << 4) => 320 MHz
   --
   -- Pineview: read from GCFGC PCI register (0xF0)
   --   GC_DISPLAY_CLOCK_267_MHZ_PNV (0 << 4) => 267 MHz
   --   GC_DISPLAY_CLOCK_333_MHZ_PNV (1 << 4) => 333 MHz
   --   GC_DISPLAY_CLOCK_444_MHZ_PNV (2 << 4) => 444 MHz
   --   GC_DISPLAY_CLOCK_200_MHZ_PNV (5 << 4) => 200 MHz
   --   GC_DISPLAY_CLOCK_133_MHZ_PNV (6 << 4) => 133 MHz
   --   GC_DISPLAY_CLOCK_167_MHZ_PNV (7 << 4) => 167 MHz

   procedure Get_CDClk (CDClk : out Config.CDClk_Range)
   is
      use type HW.Word16;

      GC_LOW_FREQUENCY_ENABLE      : constant Word16 := 1 * 2 ** 7;
      GC_DISPLAY_CLOCK_MASK        : constant Word16 := 7 * 2 ** 4;
      GC_DISPLAY_CLOCK_320_MHZ     : constant Word16 := 4 * 2 ** 4;
      GC_DISPLAY_CLOCK_267_MHZ_PNV : constant Word16 := 0 * 2 ** 4;
      GC_DISPLAY_CLOCK_333_MHZ_PNV : constant Word16 := 1 * 2 ** 4;
      GC_DISPLAY_CLOCK_444_MHZ_PNV : constant Word16 := 2 * 2 ** 4;
      GC_DISPLAY_CLOCK_200_MHZ_PNV : constant Word16 := 5 * 2 ** 4;
      GC_DISPLAY_CLOCK_133_MHZ_PNV : constant Word16 := 6 * 2 ** 4;
      GC_DISPLAY_CLOCK_167_MHZ_PNV : constant Word16 := 7 * 2 ** 4;

      GCFGC : Word16;
   begin
      if Config.CPU_Any_Pineview then
         PCI_Read16 (GCFGC, 16#f0#);
         CDClk :=
           (case GCFGC and GC_DISPLAY_CLOCK_MASK is
               when GC_DISPLAY_CLOCK_267_MHZ_PNV => 266_666_667,
               when GC_DISPLAY_CLOCK_333_MHZ_PNV => 333_333_333,
               when GC_DISPLAY_CLOCK_444_MHZ_PNV => 444_444_444,
               when GC_DISPLAY_CLOCK_200_MHZ_PNV => 200_000_000,
               when GC_DISPLAY_CLOCK_167_MHZ_PNV => 166_666_667,
               when others                       => 133_333_333);
      elsif Config.GMCH_I945GM then
         PCI_Read16 (GCFGC, 16#f0#);
         if (GCFGC and GC_LOW_FREQUENCY_ENABLE) /= 0 then
            CDClk := 133_333_333;
         elsif (GCFGC and GC_DISPLAY_CLOCK_MASK) = GC_DISPLAY_CLOCK_320_MHZ then
            CDClk := 320_000_000;
         else
            CDClk := 200_000_000;
         end if;
      else
         -- i945G desktop: fixed 400 MHz
         CDClk := 400_000_000;
      end if;
   end Get_CDClk;

   -- hrawclk = FSB / 4. The CLKCFG FSB encoding differs between
   -- mobile/Pineview and desktop (i945G), see CLKCFG_FSB_MASK comment above.
   procedure Get_Raw_Clock (Raw_Clock : out Frequency_Type)
   is
      CLK_CFG : Word32;
      type Freq_Sel is new Natural range 0 .. 7;
   begin
      Registers.Read
        (Register => Registers.GMCH_CLKCFG,
         Value => CLK_CFG);
      if Config.GMCH_Gen3_Mobile or Config.CPU_Any_Pineview then
         Raw_Clock := (case Freq_Sel (CLK_CFG and CLKCFG_FSB_MASK) is
            when 0      => HRAWCLK_100,  -- FSB 400
            when 1      => HRAWCLK_133,  -- FSB 533
            when 2      => HRAWCLK_200,  -- FSB 800
            when 3      => HRAWCLK_167,  -- FSB 667
            when 6      => HRAWCLK_267,  -- FSB 1067
            when 7      => HRAWCLK_333,  -- FSB 1333
            when others => HRAWCLK_133);
      else
         Raw_Clock := (case Freq_Sel (CLK_CFG and CLKCFG_FSB_MASK) is
            when 0      => HRAWCLK_267,  -- FSB 1067
            when 1      => HRAWCLK_133,  -- FSB 533
            when 2      => HRAWCLK_200,  -- FSB 800
            when 3      => HRAWCLK_167,  -- FSB 667
            when 4      => HRAWCLK_333,  -- FSB 1333
            when 5      => HRAWCLK_100,  -- FSB 400
            when 6      => HRAWCLK_400,  -- FSB 1600
            when others => HRAWCLK_133);
      end if;
   end Get_Raw_Clock;

   procedure Initialize
   is
      CDClk   : Config.CDClk_Range;
      Raw_Clk : Frequency_Type;
   begin
      Get_CDClk (CDClk);
      Config.CDClk := CDClk;
      Config.Max_CDClk := CDClk;

      Get_Raw_Clock (Raw_Clk);
      Config.Raw_Clock := Raw_Clk;
   end Initialize;

   procedure Limit_Dotclocks
     (Configs        : in out Pipe_Configs;
      CDClk_Switch   :    out Boolean)
   is
   begin
      Config_Helpers.Limit_Dotclocks (Configs, Config.CDClk * 90 / 100);
      CDClk_Switch := False;
   end Limit_Dotclocks;

   procedure Power_Up (Port : Active_Port_Type; Success : out Boolean) is
   begin
      Success := True;
   end Power_Up;

end HW.GFX.GMA.Power_And_Clocks;
