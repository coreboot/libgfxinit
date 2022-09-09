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

with HW.GFX.GMA.Config;
with HW.GFX.GMA.Registers;
with HW.GFX.GMA.Power_And_Clocks;

package body HW.GFX.GMA.PLLs.Combo_Phy is

   subtype HDMI_Clock_Range is Frequency_Type range
      25_000_000 .. Config.HDMI_Max_Clock_24bpp;
   subtype DCO_Range is Pos64 range
      7_998_000_000 .. 10_000_000_000;

   type PLL_Regs_Record is record
      DPLL_ENABLE : Registers.Registers_Index;
      DPLL_CFGCR0 : Registers.Registers_Index;
      DPLL_CFGCR1 : Registers.Registers_Index;
      DPLL_SSC    : Registers.Registers_Index;
   end record;
   type PLL_Regs_Array is array (Combo_DPLLs) of PLL_Regs_Record;
   PLL_Regs : constant PLL_Regs_Array :=
      PLL_Regs_Array'
     (DPLL0 =>
        (Registers.DPLL_0_ENABLE,
         Registers.DPLL_0_CFGCR0,
         Registers.DPLL_0_CFGCR1,
         Registers.DPLL_0_SSC),
      DPLL1 =>
        (Registers.DPLL_1_ENABLE,
         Registers.DPLL_1_CFGCR0,
         Registers.DPLL_1_CFGCR1,
         Registers.DPLL_1_SSC));

   DPLL_ENABLE_PLL_ENABLE   : constant := 1 * 2 ** 31;
   DPLL_ENABLE_PLL_LOCK     : constant := 1 * 2 ** 30;
   DPLL_ENABLE_POWER_ENABLE : constant := 1 * 2 ** 27;
   DPLL_ENABLE_POWER_STATE  : constant := 1 * 2 ** 26;
   DPLL_SSC_DP              : constant := 16#200#;

   procedure Encode_DCO (DCO_Integer, DCO_Fraction : out Word32; DCO : DCO_Range)
   is
      Refclk_Freq : Power_And_Clocks.Refclk_Range;
      Enc_DCO : Int64;
   begin
      Power_And_Clocks.Get_Refclk (Refclk_Freq);

      -- DPLL will auto-divide by 2 if refclk is 38.4 MHz
      if Refclk_Freq = 38_400_000 then
         Refclk_Freq := 19_200_000;
      end if;

      Enc_DCO := (DCO / 1_000) * (2 ** 15) / (Refclk_Freq / 1_000);
      DCO_Integer := Word32 (Enc_DCO / (2 ** 15));
      DCO_Fraction := Word32 (Enc_DCO) and 16#7fff#;
   end Encode_DCO;

   subtype PDiv_Range is Positive range 2 .. 7
   with
      Static_Predicate => (PDiv_Range in 2 | 3 | 5 | 7);

   type Encoded_PDiv is new Positive range 1 .. 8
   with
      Static_Predicate => (Encoded_PDiv in 1 | 2 | 4 | 8);

   function Encode_PDiv (PDiv : PDiv_Range) return Encoded_PDiv
   is
     (case PDiv is
         when 2 => 2#0001#,
         when 3 => 2#0010#,
         when 5 => 2#0100#,
         when 7 => 2#1000#);

   subtype QDiv_Range is Positive range 1 .. 255;

   type Encoded_QDiv is new Natural range 0 .. QDiv_Range'Last;

   function Encode_QDiv (QDiv : QDiv_Range) return Encoded_QDiv
   is
     (Encoded_QDiv (QDiv));

   function QDiv_Mode (QDiv : Encoded_QDiv) return Natural
   is
     (if QDiv <= 1 then 0 else 1);

   subtype KDiv_Range is Positive range 1 .. 3;

   type Encoded_KDiv is new Positive range 1 .. 4
   with
      Static_Predicate => (Encoded_KDiv in 1 | 2 | 4);

   function Encode_KDiv (KDiv : KDiv_Range) return Encoded_KDiv
   is
     (case KDiv is
         when 1 => 2#001#,
         when 2 => 2#010#,
         when 3 => 2#100#);

   type PLL_Params is record
      DCO_Integer    : Word32;
      DCO_Fraction   : Word32;
      PDiv           : Encoded_PDiv;
      KDiv           : Encoded_KDiv;
      QDiv           : Encoded_QDiv;
   end record;

   type DP_PLL_Params_Array is array (DP_Bandwidth) of PLL_Params;

   PLL_Params_19_2MHz : constant DP_PLL_Params_Array := DP_PLL_Params_Array'
     (DP_Bandwidth_5_4 =>
        (DCO_Integer  => 16#1a5#,
         DCO_Fraction => 16#7000#,
         PDiv         => 2,
         KDiv         => 1,
         QDiv         => 0),
      DP_Bandwidth_2_7 =>
        (DCO_Integer  => 16#1a5#,
         DCO_Fraction => 16#7000#,
         PDiv         => 2,
         KDiv         => 2,
         QDiv         => 0),
      DP_Bandwidth_1_62 =>
        (DCO_Integer  => 16#1a5#,
         DCO_Fraction => 16#7000#,
         PDiv         => 4,
         KDiv         => 2,
         QDiv         => 0));

   PLL_Params_24MHz : constant DP_PLL_Params_Array := DP_PLL_Params_Array'
     (DP_Bandwidth_5_4 =>
        (DCO_Integer  => 16#151#,
         DCO_Fraction => 16#4000#,
         PDiv         => 2,
         KDiv         => 1,
         QDiv         => 0),
      DP_Bandwidth_2_7 =>
        (DCO_Integer  => 16#151#,
         DCO_Fraction => 16#4000#,
         PDiv         => 2,
         KDiv         => 2,
         QDiv         => 0),
      DP_Bandwidth_1_62 =>
        (DCO_Integer  => 16#151#,
         DCO_Fraction => 16#4000#,
         PDiv         => 4,
         KDiv         => 2,
         QDiv         => 0));

   procedure Calc_DP_PLL_Dividers
     (Bandwidth : in     DP_Bandwidth;
      Params    :    out PLL_Params)
   is
      Refclk : Frequency_Type;
   begin
      Power_And_Clocks.Get_Refclk (Refclk);
      if Refclk = 24_000_000 then
         Params := PLL_Params_24MHz (Bandwidth);
      else
         Params := PLL_Params_19_2MHz (Bandwidth);
      end if;
   end Calc_DP_PLL_Dividers;

   procedure Calc_HDMI_PLL_Dividers
     (Dotclock : in     Frequency_Type;
      Params   :    out PLL_Params;
      Success  :    out Boolean)
   is
      subtype Div_Range is Pos64 range 2 .. 102;
      subtype Candidate_Index is Positive range 1 .. 46;
      type Candidate_Array is array (Candidate_Index) of Div_Range;
      Candidates : constant Candidate_Array := Candidate_Array'
        (2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 24, 28, 30, 32, 36, 40, 42, 44,
         48, 50, 52, 54, 56, 60, 64, 66, 68, 70, 72, 76, 78, 80, 84, 88, 90,
         92, 96, 98, 100, 102, 3, 5, 7, 9, 15, 21);
      AFE_Clk : constant Int64 := Dotclock * 5;
      DCO_Mid : constant Int64 := (DCO_Range'First + DCO_Range'Last) / 2;
      Best_DCO_Centrality : Int64 := Frequency_Type'Last;
      Best_Div_Index : Candidate_Index := Candidate_Index'First;
      Best_Div : Div_Range;
      Best_DCO : DCO_Range := DCO_Range'First;
      DCO_Found : Boolean := False;
      PDiv : PDiv_Range;
      QDiv : QDiv_Range;
      KDiv : KDiv_Range;
   begin
      for Index in Candidate_Index loop
         declare
            DCO : constant Int64 := AFE_Clk * Candidates(Index);
            DCO_Centrality : constant Int64 := abs (DCO - DCO_Mid);
         begin
            if DCO <= DCO_Range'Last and DCO >= DCO_Range'First and
               DCO_Centrality < Best_DCO_Centrality
            then
               DCO_Found := True;
               Best_DCO_Centrality := DCO_Centrality;
               Best_Div_Index := Index;
               Best_DCO := DCO;
            end if;
         end;
      end loop;

      if not DCO_Found then
         Params := (DCO_Integer => 0,
                    DCO_Fraction => 0,
                    PDiv => Encoded_PDiv'First_Valid,
                    KDiv => Encoded_KDiv'First_Valid,
                    QDiv => Encoded_QDiv'First_Valid);
         Success := False;
         return;
      end if;

      Best_Div := Candidates (Best_Div_Index);
      if Best_Div mod 2 = 0 then
         if Best_Div = 2 then
            PDiv := 2;
            QDiv := 1;
            KDiv := 1;
         elsif Best_Div mod 4 = 0 then
            PDiv := 2;
            QDiv := QDiv_Range (Best_Div / 4);
            KDiv := 2;
         elsif Best_Div mod 6 = 0 then
            PDiv := 3;
            QDiv := QDiv_Range (Best_Div / 6);
            KDiv := 2;
         elsif Best_Div mod 5 = 0 then
            PDiv := 5;
            QDiv := QDiv_Range (Best_Div / 10);
            KDiv := 2;
         else
            -- Use `else`, not `elsif`, to prove we covered all cases.
            pragma Assert (Best_Div mod 14 = 0);
            PDiv := 7;
            QDiv := QDiv_Range (Best_Div / 14);
            KDiv := 2;
         end if;
      else
         if Best_Div = 3 or Best_Div = 5 or Best_Div = 7 then
            PDiv := PDiv_Range (Best_Div);
            QDiv := 1;
            KDiv := 1;
         else
            pragma Assert (Best_Div mod 3 = 0);
            PDiv := PDiv_Range (Best_Div / 3);
            QDiv := 1;
            KDiv := 3;
         end if;
      end if;

      -- PRM: If Kdiv != 2, then Qdiv must be 1. Else Qdiv can be 1 to 255.
      pragma Assert (if KDiv /= 2 then QDiv = 1);

      Params.KDiv := Encode_KDiv (KDiv);
      Params.PDiv := Encode_PDiv (PDiv);
      Params.QDiv := Encode_QDiv (QDiv);
      Encode_DCO (Params.DCO_Integer, Params.DCO_Fraction, Best_DCO);

      Success := True;
   end Calc_HDMI_PLL_Dividers;

   procedure On
     (PLL      : in     Combo_DPLLs;
      Port_Cfg : in     Port_Config;
      Success  :    out Boolean)
   is
      Params : PLL_Params;
      Refclk : Frequency_Type;
   begin
      if Port_Cfg.Display = DP then
         Calc_DP_PLL_Dividers (Port_Cfg.DP.Bandwidth, Params);
         Success := True;
      else
         if Port_Cfg.Mode.Dotclock not in HDMI_Clock_Range then
            Debug.Put_Line ("Unsupported HDMI Pixel clock");
            Success := False;
            return;
         end if;
         declare
            Color_Depth : constant Int64 := Port_Cfg.Mode.BPC * 3;
            Pll_Freq : constant Frequency_Type := Color_Depth * Port_Cfg.Mode.Dotclock / 24;
         begin
            Calc_HDMI_PLL_Dividers (Pll_Freq, Params, Success);
         end;
      end if;

      if not Success then
         Debug.Put_Line ("Failed to calculate PLL dividers!");
         return;
      end if;

      -- Display WA #22010492432: ehl, tgl, adl-p
      -- Program half of the nominal DCO divider fraction value
      -- for 38.4 MHz refclk
      Power_And_Clocks.Get_Refclk (Refclk);
      if Refclk = 38_400_000 then
         Params.DCO_Fraction := Shift_Right (Params.DCO_Fraction, 1);
      end if;

      Registers.Set_Mask
        (Register => PLL_Regs (PLL).DPLL_ENABLE,
         Mask     => DPLL_ENABLE_POWER_ENABLE);
      Registers.Wait_Set_Mask
        (Register => PLL_Regs (PLL).DPLL_ENABLE,
         Mask     => DPLL_ENABLE_POWER_STATE,
         Success  => Success);

      if not Success then
         Debug.Put_Line ("Failed to enable PLL!");
         return;
      end if;

      -- Configure DPLL_SSC
      Registers.Write
        (Register => PLL_Regs (PLL).DPLL_SSC,
         Value    => (if Port_Cfg.Display = DP then DPLL_SSC_DP else 0));

      Registers.Write
        (Register => PLL_Regs (PLL).DPLL_CFGCR0,
         Value => Shift_Left (Params.DCO_Fraction, 10) or
                  Params.DCO_Integer);

      Registers.Write
        (Register => PLL_Regs (PLL).DPLL_CFGCR1,
         Value => Shift_Left (Word32 (Params.QDiv), 10) or
                  Shift_Left (Word32 (QDiv_Mode (Params.QDiv)), 9) or
                  Shift_left (Word32 (Params.KDiv), 6) or
                  Shift_Left (Word32 (Params.PDiv), 2));
      Registers.Posting_Read(PLL_Regs (PLL).DPLL_CFGCR1);

      -- Enable DPLL
      Registers.Set_Mask
        (Register => PLL_Regs (PLL).DPLL_ENABLE,
         Mask     => DPLL_ENABLE_PLL_ENABLE);
      -- Wait for PLL Lock status
      Registers.Wait_Set_Mask
        (Register => PLL_Regs (PLL).DPLL_ENABLE,
         Mask     => DPLL_ENABLE_PLL_LOCK,
         Success  => Success);
   end On;

   procedure Free (PLL : Combo_DPLLs)
   is
   begin
      Registers.Unset_Mask
        (Register => PLL_Regs (PLL).DPLL_ENABLE,
         Mask     => DPLL_ENABLE_PLL_ENABLE);
      Registers.Wait_Unset_Mask
        (Register => PLL_Regs (PLL).DPLL_ENABLE,
         Mask     => DPLL_ENABLE_PLL_LOCK);

      Registers.Unset_Mask
        (Register => PLL_Regs (PLL).DPLL_ENABLE,
         Mask     => DPLL_ENABLE_POWER_ENABLE);
      Registers.Wait_Unset_Mask
        (Register => PLL_Regs (PLL).DPLL_ENABLE,
         Mask     => DPLL_ENABLE_POWER_STATE);
   end Free;

   procedure All_Off is
   begin
      for PLL in Combo_DPLLs loop
         Free (PLL);
      end loop;
   end All_Off;

end HW.GFX.GMA.PLLs.Combo_Phy;
