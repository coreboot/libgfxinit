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

package body HW.GFX.GMA.PLLs.Dekel_Phy is

   use type HW.Word64;

   subtype Frequency_KHz is Pos64
     range (Frequency_Type'First / 1_000) .. (Frequency_Type'Last / 1_000);
   subtype DCO_Range_KHz is Pos64 range 7_992_000 .. 10_000_000;

   DPLL_ENABLE_PLL_ENABLE   : constant := 1 * 2 ** 31;
   DPLL_ENABLE_PLL_LOCK     : constant := 1 * 2 ** 30;
   DPLL_ENABLE_POWER_ENABLE : constant := 1 * 2 ** 27;
   DPLL_ENABLE_POWER_STATE  : constant := 1 * 2 ** 26;

   function HIP_INDEX_REG (P : DKL_DPLLs) return Registers.Registers_Index
   is (if P <= TCPLL4
       then Registers.HIP_INDEX_REG0
       else Registers.HIP_INDEX_REG1);

   function HIP_INDEX_VAL (P : DKL_DPLLs; Val : Word32) return Word32 is
     (case P is
      when TCPLL1 => Val * 2 ** 0,
      when TCPLL2 => Val * 2 ** 8,
      when TCPLL3 => Val * 2 ** 16,
      when TCPLL4 => Val * 2 ** 24,
      when TCPLL5 => Val * 2 ** 0,
      when TCPLL6 => Val * 2 ** 8);

   function DKL_PLL_ENABLE (P : DKL_DPLLs) return Registers.Registers_Index is
     (if Config.Has_New_Type_C_PLL_Enable then
        (case P is
            when TCPLL1 => Registers.PORTTC1_PLL1_ENABLE,
            when TCPLL2 => Registers.PORTTC2_PLL1_ENABLE,
            when TCPLL3 => Registers.PORTTC3_PLL1_ENABLE,
            when TCPLL4 => Registers.PORTTC4_PLL1_ENABLE,
            when TCPLL5 => Registers.MGPLL5_ENABLE,
            when TCPLL6 => Registers.MGPLL6_ENABLE)
      else
        (case P is
            when TCPLL1 => Registers.MGPLL1_ENABLE,
            when TCPLL2 => Registers.MGPLL2_ENABLE,
            when TCPLL3 => Registers.MGPLL3_ENABLE,
            when TCPLL4 => Registers.MGPLL4_ENABLE,
            when TCPLL5 => Registers.MGPLL5_ENABLE,
            when TCPLL6 => Registers.MGPLL6_ENABLE));

   type PLL_Regs_Record is record
      DKL_REFCLKIN_CTL        : Registers.Registers_Index;
      DKL_CLKTOP2_CORECLKCTL1 : Registers.Registers_Index;
      DKL_CLKTOP2_HSCLKCTL    : Registers.Registers_Index;
      DKL_PLL_DIV0            : Registers.Registers_Index;
      DKL_PLL_DIV1            : Registers.Registers_Index;
      DKL_PLL_SSC             : Registers.Registers_Index;
      DKL_PLL_BIAS            : Registers.Registers_Index;
      DKL_PLL_COLDST_BIAS     : Registers.Registers_Index;
   end record;

   type PLL_Regs_Array is array (DKL_DPLLs) of PLL_Regs_Record;
   PLL_Regs : constant PLL_Regs_Array :=
      PLL_Regs_Array'
     (TCPLL1 =>
        (Registers.DKL_REFCLKIN_CTL_1,
         Registers.DKL_CLKTOP2_CCC1_1,
         Registers.DKL_CLKTOP2_HSCC_1,
         Registers.DKL_PLL_DIV0_1,
         Registers.DKL_PLL_DIV1_1,
         Registers.DKL_PLL_SSC_1,
         Registers.DKL_PLL_BIAS_1,
         Registers.DKL_PLL_COLDST_BIAS_1),
      TCPLL2 =>
        (Registers.DKL_REFCLKIN_CTL_2,
         Registers.DKL_CLKTOP2_CCC1_2,
         Registers.DKL_CLKTOP2_HSCC_2,
         Registers.DKL_PLL_DIV0_2,
         Registers.DKL_PLL_DIV1_2,
         Registers.DKL_PLL_SSC_2,
         Registers.DKL_PLL_BIAS_2,
         Registers.DKL_PLL_COLDST_BIAS_2),
      TCPLL3 =>
        (Registers.DKL_REFCLKIN_CTL_3,
         Registers.DKL_CLKTOP2_CCC1_3,
         Registers.DKL_CLKTOP2_HSCC_3,
         Registers.DKL_PLL_DIV0_3,
         Registers.DKL_PLL_DIV1_3,
         Registers.DKL_PLL_SSC_3,
         Registers.DKL_PLL_BIAS_3,
         Registers.DKL_PLL_COLDST_BIAS_3),
      TCPLL4 =>
        (Registers.DKL_REFCLKIN_CTL_4,
         Registers.DKL_CLKTOP2_CCC1_4,
         Registers.DKL_CLKTOP2_HSCC_4,
         Registers.DKL_PLL_DIV0_4,
         Registers.DKL_PLL_DIV1_4,
         Registers.DKL_PLL_SSC_4,
         Registers.DKL_PLL_BIAS_4,
         Registers.DKL_PLL_COLDST_BIAS_4),
      TCPLL5 =>
        (Registers.DKL_REFCLKIN_CTL_5,
         Registers.DKL_CLKTOP2_CCC1_5,
         Registers.DKL_CLKTOP2_HSCC_5,
         Registers.DKL_PLL_DIV0_5,
         Registers.DKL_PLL_DIV1_5,
         Registers.DKL_PLL_SSC_5,
         Registers.DKL_PLL_BIAS_5,
         Registers.DKL_PLL_COLDST_BIAS_5),
      TCPLL6 =>
        (Registers.DKL_REFCLKIN_CTL_6,
         Registers.DKL_CLKTOP2_CCC1_6,
         Registers.DKL_CLKTOP2_HSCC_6,
         Registers.DKL_PLL_DIV0_6,
         Registers.DKL_PLL_DIV1_6,
         Registers.DKL_PLL_SSC_6,
         Registers.DKL_PLL_BIAS_6,
         Registers.DKL_PLL_COLDST_BIAS_6));

   DKL_REFCLKIN_CTL_OD_2_MUX_MASK          : constant := 7 * 2 ** 8;
   DKL_CLKTOP2_CORECLKCTL1_A_DIVRATIO_MASK : constant := 16#ff# * 2 ** 8;
   DKL_CLKTOP2_HSCLKCTL_MASK               : constant := 16#1_FF00#;
   DKL_PLL_DIV0_MASK                       : constant := 16#1F_FFFF#;
   DKL_PLL_DIV1_MASK                       : constant := 16#1F_00FF#;
   DKL_PLL_SSC_MASK                        : constant := 16#E0FF_3A00#;
   DKL_PLL_BIAS_MASK                       : constant := 16#7FFF_FF00#;
   DKL_PLL_COLD_BIAS_MASK                  : constant := 16#ffff#;
   DKL_PLL_BIAS_FRAC_EN_H                  : constant := 1 * 2 ** 30;

   procedure Calc_Dividers
      (Clock                   : in     Frequency_Type;
       Display                 : in     Display_Type;
       DKL_Refclkin_Ctl        :    out Word32;
       DKL_Clktop2_Coreclkctl1 :    out Word32;
       DKL_Clktop2_HSClkCtl    :    out Word32;
       DCO_Khz                 :    out DCO_Range_KHz;
       Success                 :    out Boolean)
   is
      DCO_Min_Freq, DCO_Max_Freq : Int64;
      type Dividers_List is array (1 .. 4) of Int64;
      Dividers : constant Dividers_List := (7, 5, 3, 2);
      DKL_CLKTOP2_HSCLKCTL_HSDIV_RATIO_2 : constant := 0 * 2 ** 12;
      DKL_CLKTOP2_HSCLKCTL_HSDIV_RATIO_3 : constant := 1 * 2 ** 12;
      DKL_CLKTOP2_HSCLKCTL_HSDIV_RATIO_5 : constant := 2 * 2 ** 12;
      DKL_CLKTOP2_HSCLKCTL_HSDIV_RATIO_7 : constant := 3 * 2 ** 12;
      function DKL_REFCLKIN_CTL_OD_2_MUX (N : Word32) return Word32
         is (Shift_Left (N, 8));
      function DKL_CLKTOP2_CORECLKCTL1_A_DIVRATIO  (N : Word32) return Word32
         is (Shift_Left (N, 8));
      function DKL_CLKTOP2_HSCLKCTL_TLINEDRV_CLKSEL (N : Word32) return Word32
         is (Shift_Left (N, 14));
      function DKL_CLKTOP2_HSCLKCTL_CORE_INPUTSEL (N : Word32) return Word32
         is (Shift_Left (N, 16));
      function DKL_CLKTOP2_HSCLKCTL_DSDIV_RATIO (N : Word32) return Word32
         is (Shift_Left (N, 8));
      A_DivRatio, TLineDrv, Inputsel, Hsdiv : Word32;
      Clock_Khz : constant Frequency_KHz := Frequency_KHz(Clock / 1_000);
   begin
      if Display = DP then
         DCO_Min_Freq := 8_100_000;
         DCO_Max_Freq := 8_100_000;
      else
         DCO_Min_Freq := DCO_Range_KHz'First;
         DCO_Max_Freq := DCO_Range_KHz'Last;
      end if;

      DKL_Refclkin_Ctl := 0;
      DKL_clktop2_coreclkctl1 := 0;
      DKL_CLKTOP2_hsclkctl := 0;

      DCO_KHz := DCO_Min_Freq;
      Success := False;
      for I in Dividers'Range loop
         pragma Loop_Invariant (DCO_KHz >= DCO_Min_Freq and DCO_KHz <= DCO_Max_Freq);
         for Div2 in reverse 1 .. 10 loop
            declare
               Tmp: constant Pos64 := Dividers (I) * Pos64 (Div2) * Pos64(Clock_Khz) * 5;
            begin
               if Tmp >= DCO_Min_Freq and then Tmp <= DCO_Max_Freq then
                  DCO_KHz := Tmp;
                  if Div2 >= 2 then
                     A_DivRatio := (if Display = DP then 10 else 5);
                     TLineDrv := 1;
                  else
                     A_DivRatio := 5;
                     TLineDrv := 0;
                  end if;
                  Inputsel := (if Display = DP then 0 else 1);
                  Hsdiv := (case Dividers (I) is
                            when 2 => DKL_CLKTOP2_HSCLKCTL_HSDIV_RATIO_2,
                            when 3 => DKL_CLKTOP2_HSCLKCTL_HSDIV_RATIO_3,
                            when 5 => DKL_CLKTOP2_HSCLKCTL_HSDIV_RATIO_5,
                            when 7 => DKL_CLKTOP2_HSCLKCTL_HSDIV_RATIO_7,
                            when others => DKL_CLKTOP2_HSCLKCTL_HSDIV_RATIO_2);
                  DKL_Refclkin_Ctl := DKL_REFCLKIN_CTL_OD_2_MUX (1);
                  DKL_clktop2_coreclkctl1 :=
                                DKL_CLKTOP2_CORECLKCTL1_A_DIVRATIO (A_DivRatio);
                  DKL_CLKTOP2_hsclkctl :=
                     DKL_CLKTOP2_HSCLKCTL_TLINEDRV_CLKSEL (TLineDrv) or
                     DKL_CLKTOP2_HSCLKCTL_CORE_INPUTSEL (Inputsel) or
                     Hsdiv or
                     DKL_CLKTOP2_HSCLKCTL_DSDIV_RATIO (Word32 (Div2));
                  Success := True;
               end if;
            exit when Success;
            end;
         end loop;
         exit when Success;
      end loop;
   end Calc_Dividers;

   procedure On
     (PLL      : in     DKL_DPLLs;
      Port_Cfg : in     Port_Config;
      Success  :    out Boolean)
   is
      DKL_Refclkin_Ctl : Word32;
      DKL_Clktop2_Coreclkctl1 : Word32;
      DKL_Clktop2_HSClkCtl : Word32;
      DCO_Khz : DCO_Range_KHz;
      Clock : Frequency_Type;
   begin
      if Port_Cfg.Display = HDMI then
         Clock := Port_Cfg.Mode.Dotclock;
      else
         Clock := Frequency_Type (DP_Symbol_Rate (Port_Cfg.DP.Bandwidth));
      end if;

      Calc_Dividers
        (Clock                   => Clock,
         Display                 => Port_Cfg.Display,
         DKL_Refclkin_Ctl        => DKL_Refclkin_Ctl,
         DKL_Clktop2_Coreclkctl1 => DKL_Clktop2_Coreclkctl1,
         DKL_Clktop2_HSClkCtl    => DKL_Clktop2_HSClkCtl,
         DCO_Khz                 => DCO_Khz,
         Success                 => Success);

      if not Success then
         Debug.Put_Line ("Could not find dividers for port!");
         return;
      end if;

      Registers.Set_Mask
        (Register => DKL_PLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_POWER_ENABLE);

      Registers.Wait_Set_Mask
        (Register => DKL_PLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_POWER_STATE);

      declare
         Tmp : Int64;
         Refclk : Power_And_Clocks.Refclk_Range;
         Refclk_Khz : Power_And_Clocks.Refclk_Range_KHz;
         FeedFwGain : Word32;
         M1Div : constant := 2;
         M2Div_Int, M2Div_Rem: Int64;
         M2Div_Frac : Word32;
         TDC_Target : Word32;
         Prop_Coeff, Int_Coeff : Word32;
         IRef_Ndiv, Iref_Itrim : Word32;
      begin
         Power_And_Clocks.Get_Refclk (Refclk);
         Refclk_Khz := Power_And_Clocks.Refclk_Range_KHz (Refclk / 1_000);
         M2Div_Int := Int64(DCO_Khz / (Refclk_Khz * M1Div));
         M2Div_Rem := Int64(DCO_Khz rem (Refclk_Khz * M1Div));
         Tmp := M2Div_Rem * 2 ** 22;
         M2Div_Frac := Word32 (Tmp / Int64(Refclk_Khz * M1Div));

         case Refclk is
            when 24_000_000 =>
               Iref_Ndiv := 1;
               Iref_Itrim := 25;
            when 38_400_000 =>
               Iref_Ndiv := 2;
               Iref_Itrim := 28;
            when others =>
               Iref_Ndiv := 1;
               Iref_Itrim := 28;
         end case;

         -- Real number math converted to fixed point
         -- see note in i915 about these calculations
         TDC_Target := Word32 (2 * 1_000 * 100_000 * 10 / (132 * Refclk_Khz) + 5) / 10;

         if M2Div_Rem > 0 then
            FeedFwGain := (M1Div * 1_000_000 * 100) / (Word32 (DCO_Khz) * 3 / 10);
         else
            FeedFwGain := 0;
         end if;

         if DCO_Khz >= 9_000_000 then
            Prop_Coeff := 5;
            Int_Coeff := 10;
         else
            Prop_Coeff := 4;
            Int_Coeff := 8;
         end if;

         -- The following DKL registers are at MMIO index 2
         Registers.Write (HIP_INDEX_REG (PLL), HIP_INDEX_VAL (PLL, 2));

         Registers.Unset_And_Set_Mask
           (Register   => PLL_Regs (PLL).DKL_REFCLKIN_CTL,
            Mask_Unset => DKL_REFCLKIN_CTL_OD_2_MUX_MASK,
            Mask_Set   => DKL_Refclkin_Ctl);

         Registers.Unset_And_Set_Mask
           (Register   => PLL_Regs (PLL).DKL_CLKTOP2_CORECLKCTL1,
            Mask_Unset => DKL_CLKTOP2_CORECLKCTL1_A_DIVRATIO_MASK,
            Mask_Set   => DKL_Clktop2_Coreclkctl1);

         Registers.Unset_And_Set_Mask
           (Register   => PLL_Regs (PLL).DKL_CLKTOP2_HSCLKCTL,
            Mask_Unset => DKL_CLKTOP2_HSCLKCTL_MASK,
            Mask_Set   => DKL_CLKTOP2_hsclkctl);

         Registers.Unset_And_Set_mask
           (Register   => PLL_Regs (PLL).DKL_PLL_DIV0,
            Mask_Unset => DKL_PLL_DIV0_MASK,
            Mask_Set   => Shift_Left (Int_Coeff, 16) or
                          Shift_Left (Prop_Coeff, 12) or
                          Shift_Left (M1Div, 8) or
                          Word32(M2Div_Int));

         Registers.Unset_And_Set_Mask
           (Register   => PLL_Regs (PLL).DKL_PLL_DIV1,
            Mask_Unset => DKL_PLL_DIV1_MASK,
            Mask_Set   => Shift_Left (Iref_Itrim, 16) or
                          TDC_Target);

         Registers.Unset_And_Set_Mask
           (Register   => PLL_Regs (PLL).DKL_PLL_SSC,
            Mask_Unset => DKL_PLL_SSC_MASK,
            Mask_Set   => Shift_Left (Iref_Ndiv, 29) or
                          Shift_Left (4, 11)); -- SSC_STEP_NUM (always 4)

         Registers.Unset_And_Set_Mask
           (Register   => PLL_Regs (PLL).DKL_PLL_BIAS,
            Mask_Unset => DKL_PLL_BIAS_MASK,
            Mask_Set   => Shift_Left (M2Div_Frac, 8) or
                          (if M2Div_Frac > 0 then DKL_PLL_BIAS_FRAC_EN_H else 0));

         Registers.Unset_And_Set_Mask
           (Register   => PLL_Regs (PLL).DKL_PLL_COLDST_BIAS,
            Mask_Unset => DKL_PLL_COLD_BIAS_MASK,
            Mask_Set   => FeedFwGain);

         -- Read back the last programmed PHY PLL register
         Registers.Posting_Read (PLL_Regs (PLL).DKL_PLL_COLDST_BIAS);

         Registers.Set_Mask
           (Register => DKL_PLL_ENABLE (PLL),
            Mask     => DPLL_ENABLE_PLL_ENABLE);

         Registers.Wait_Set_Mask
           (Register => DKL_PLL_ENABLE (PLL),
            Mask     => DPLL_ENABLE_PLL_LOCK);
      end;
   end On;

   procedure Free (PLL : DKL_DPLLs)
   is
   begin
      Registers.Unset_Mask
        (Register => DKL_PLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_PLL_ENABLE);
      Registers.Wait_Unset_Mask
        (Register => DKL_PLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_PLL_LOCK);

      Registers.Unset_Mask
        (Register => DKL_PLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_POWER_ENABLE);
      Registers.Wait_Unset_Mask
        (Register => DKL_PLL_ENABLE (PLL),
         Mask     => DPLL_ENABLE_POWER_STATE);
   end Free;

   procedure All_Off is
   begin
      for PLL in DKL_DPLLs loop
         Free (PLL);
      end loop;
   end All_Off;

end HW.GFX.GMA.PLLs.Dekel_Phy;
