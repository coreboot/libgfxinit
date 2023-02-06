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

with HW.GFX.DP_Training;
with HW.GFX.GMA.Config;
with HW.GFX.GMA.DP_Aux_Ch;
with HW.GFX.GMA.Registers;

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Connectors.Combo_Phy is

   DDI_BUF_CTL_BUFFER_ENABLE        : constant :=     1 * 2 ** 31;
   DDI_BUF_CTL_TRANS_SELECT_MASK    : constant := 16#f# * 2 ** 24;
   DDI_BUF_CTL_PORT_REVERSAL        : constant :=     1 * 2 ** 16;
   DDI_BUF_CTL_PORT_WIDTH_MASK      : constant :=     7 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_1_LANE    : constant :=     0 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_2_LANES   : constant :=     1 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_4_LANES   : constant :=     3 * 2 **  1;
   DDI_BUF_CTL_IDLE_STATUS          : constant :=     1 * 2 **  7;

   type Combo_Port_Regs is array (Combo_Port) of Registers.Registers_Index;
   DDI_BUF_CTL : constant Combo_Port_Regs :=
      Combo_Port_Regs'
     (DIGI_A  => Registers.DDI_BUF_CTL_A,
      DIGI_B  => Registers.DDI_BUF_CTL_B,
      DIGI_C  => Registers.DDI_BUF_CTL_C);

   type DDI_BUF_CTL_PORT_WIDTH_T is array (HW.GFX.DP_Lane_Count) of Word32;
   DDI_BUF_CTL_PORT_WIDTH : constant DDI_BUF_CTL_PORT_WIDTH_T :=
      DDI_BUF_CTL_PORT_WIDTH_T'
     (HW.GFX.DP_Lane_Count_1 => DDI_BUF_CTL_PORT_WIDTH_1_LANE,
      HW.GFX.DP_Lane_Count_2 => DDI_BUF_CTL_PORT_WIDTH_2_LANES,
      HW.GFX.DP_Lane_Count_4 => DDI_BUF_CTL_PORT_WIDTH_4_LANES);

   type Port_Regs_Record is record
      PORT_TX_DW5_LN0 : Registers.Registers_Index;
      PORT_TX_DW5_GRP : Registers.Registers_Index;
      PORT_TX_DW7_LN0 : Registers.Registers_Index;
      PORT_TX_DW7_GRP : Registers.Registers_Index;
      PORT_TX_DW2_LN0 : Registers.Registers_Index;
      PORT_TX_DW2_GRP : Registers.Registers_Index;
      PORT_TX_DW4_LN0 : Registers.Registers_Index;
      PORT_TX_DW4_LN1 : Registers.Registers_Index;
      PORT_TX_DW4_LN2 : Registers.Registers_Index;
      PORT_TX_DW4_LN3 : Registers.Registers_Index;
      PORT_TX_DW4_GRP : Registers.Registers_Index;
      PORT_PCS_DW1_LN0 : Registers.Registers_Index;
      PORT_PCS_DW1_GRP : Registers.Registers_Index;
      PORT_CL_DW5 : Registers.Registers_Index;
      PORT_CL_DW10 : Registers.Registers_Index;
      DDI_BUF_CTL : Registers.Registers_Index;
   end record;

   type Port_Regs_Array is array (Combo_Port) of Port_Regs_Record;
   Port_Regs : constant Port_Regs_Array :=
      Port_Regs_Array'
     (DIGI_A =>
       (Registers.PORT_TX_DW5_LN0_A, Registers.PORT_TX_DW5_GRP_A,
        Registers.PORT_TX_DW7_LN0_A, Registers.PORT_TX_DW7_GRP_A,
        Registers.PORT_TX_DW2_LN0_A, Registers.PORT_TX_DW2_GRP_A,
        Registers.PORT_TX_DW4_LN0_A, Registers.PORT_TX_DW4_LN1_A,
           Registers.PORT_TX_DW4_LN2_A, Registers.PORT_TX_DW4_LN3_A,
           Registers.PORT_TX_DW4_GRP_A,
        Registers.PORT_PCS_DW1_LN0_A, Registers.PORT_PCS_DW1_GRP_A,
        Registers.PORT_CL_DW5_A,
        Registers.PORT_CL_DW10_A,
        Registers.DDI_BUF_CTL_A),
      DIGI_B =>
        (Registers.PORT_TX_DW5_LN0_B, Registers.PORT_TX_DW5_GRP_B,
         Registers.PORT_TX_DW7_LN0_B, Registers.PORT_TX_DW7_GRP_B,
         Registers.PORT_TX_DW2_LN0_B, Registers.PORT_TX_DW2_GRP_B,
         Registers.PORT_TX_DW4_LN0_B, Registers.PORT_TX_DW4_LN1_B,
            Registers.PORT_TX_DW4_LN2_B, Registers.PORT_TX_DW4_LN3_B,
            Registers.PORT_TX_DW4_GRP_B,
         Registers.PORT_PCS_DW1_LN0_B, Registers.PORT_PCS_DW1_GRP_B,
         Registers.PORT_CL_DW5_B,
         Registers.PORT_CL_DW10_B,
         Registers.DDI_BUF_CTL_B),
      DIGI_C =>
        (Registers.PORT_TX_DW5_LN0_C, Registers.PORT_TX_DW5_GRP_C,
         Registers.PORT_TX_DW7_LN0_C, Registers.PORT_TX_DW7_GRP_C,
         Registers.PORT_TX_DW2_LN0_C, Registers.PORT_TX_DW2_GRP_C,
         Registers.PORT_TX_DW4_LN0_C, Registers.PORT_TX_DW4_LN1_C,
            Registers.PORT_TX_DW4_LN2_C, Registers.PORT_TX_DW4_LN3_C,
            Registers.PORT_TX_DW4_GRP_C,
         Registers.PORT_PCS_DW1_LN0_C, Registers.PORT_PCS_DW1_GRP_C,
         Registers.PORT_CL_DW5_C,
         Registers.PORT_CL_DW10_C,
         Registers.DDI_BUF_CTL_C));

   type Lanes is (LN0, LN1, LN2, LN3);
   type Lanes_Range is new Natural range 1 .. 4;
   function PORT_TX_DW4 (Lane : Lanes; Port : Combo_Port)
      return Registers.Registers_Index
   is (case Lane is
       when LN0 => Port_Regs (Port).PORT_TX_DW4_LN0,
       when LN1 => Port_Regs (Port).PORT_TX_DW4_LN1,
       when LN2 => Port_Regs (Port).PORT_TX_DW4_LN2,
       when LN3 => Port_Regs (Port).PORT_TX_DW4_LN3);

   PORT_PCS_DW1_LN0_COMMON_KEEPER : constant := 1 * 2 ** 26;

   type Post_Cursor is new Natural range 0 .. 63;
   function PORT_TX_DW4_POST_CURSOR1 (P : Post_Cursor) return Word32 is
      (Shift_Left (Word32 (P), 12));

   type Cursor_Coeff is new Natural range 0 .. 63;
   function PORT_TX_DW4_CURSOR_COEFF (C : Cursor_Coeff) return Word32 is
      (Word32 (C));

   type N_Scalar is new Natural range 0 .. 127;
   function PORT_TX_DW7_N_SCALAR (N : N_Scalar) return Word32 is
      (Shift_Left (Word32 (N), 24));

   type Swing_Select is new Natural range 0 .. 15;
   function PORT_TX_SWING_SEL_UPPER (S : Swing_Select) return Word32 is
      (Shift_Left (Shift_Right (Word32(S), 3), 15));
   function PORT_TX_SWING_SEL_LOWER (S : Swing_Select) return Word32 is
      (Shift_Left (Word32(S), 11));

   type Buffer_Trans is record
     DW2_SWING_SEL    : Swing_Select;
     DW7_N_SCALAR     : N_Scalar;
     DW4_CURSOR_COEFF : Cursor_Coeff;
     DW4_POST_CURSOR1 : Post_Cursor;
   end record;

   type Buffer_Trans_HDMI_Range is new Natural range 0 .. 6;
   type Buffer_Trans_HDMI_Array is array (Buffer_Trans_HDMI_Range) of Buffer_Trans;
   Buffer_Trans_HDMI : constant Buffer_Trans_HDMI_Array :=
      Buffer_Trans_HDMI_Array'(
     (16#A#, 16#60#, 16#3F#, 16#00#),
     (16#B#, 16#73#, 16#36#, 16#09#),
     (16#6#, 16#7F#, 16#31#, 16#0E#),
     (16#B#, 16#73#, 16#3F#, 16#00#),
     (16#6#, 16#7F#, 16#37#, 16#08#),
     (16#6#, 16#7F#, 16#3F#, 16#00#),
     (16#6#, 16#7F#, 16#35#, 16#0A#));

   type Buffer_Trans_DP_Range is new Natural range 0 .. 9;
   type Buffer_Trans_DP_Array is array (Buffer_Trans_DP_Range) of Buffer_Trans;

   TGL_Buffer_Trans_DP_HBR : constant Buffer_Trans_DP_Array :=
      Buffer_Trans_DP_Array'(
     (16#a#, 16#32#, 16#3f#, 16#00#),
     (16#a#, 16#4f#, 16#37#, 16#08#),
     (16#c#, 16#71#, 16#2f#, 16#10#),
     (16#6#, 16#7d#, 16#2b#, 16#14#),
     (16#a#, 16#4c#, 16#3f#, 16#00#),
     (16#c#, 16#73#, 16#34#, 16#0b#),
     (16#6#, 16#7f#, 16#2f#, 16#10#),
     (16#c#, 16#6c#, 16#3c#, 16#03#),
     (16#6#, 16#7f#, 16#35#, 16#0a#),
     (16#6#, 16#7f#, 16#3f#, 16#00#));

   TGL_Buffer_Trans_DP_HBR2 : constant Buffer_Trans_DP_Array :=
      Buffer_Trans_DP_Array'(
     (16#a#, 16#35#, 16#3f#, 16#00#),
     (16#a#, 16#4f#, 16#37#, 16#08#),
     (16#c#, 16#63#, 16#2f#, 16#10#),
     (16#6#, 16#7f#, 16#2b#, 16#14#),
     (16#a#, 16#47#, 16#3f#, 16#00#),
     (16#c#, 16#63#, 16#34#, 16#0b#),
     (16#6#, 16#7f#, 16#2f#, 16#10#),
     (16#c#, 16#61#, 16#3c#, 16#03#),
     (16#6#, 16#7b#, 16#35#, 16#0a#),
     (16#6#, 16#7f#, 16#3f#, 16#00#));

   TGL_Buffer_Trans_DP_HBR2_U_Y : constant Buffer_Trans_DP_Array :=
      Buffer_Trans_DP_Array'(
     (16#a#, 16#35#, 16#3f#, 16#00#),
     (16#a#, 16#4f#, 16#36#, 16#09#),
     (16#c#, 16#60#, 16#32#, 16#0d#),
     (16#c#, 16#7f#, 16#2d#, 16#12#),
     (16#c#, 16#47#, 16#3f#, 16#00#),
     (16#c#, 16#6f#, 16#36#, 16#09#),
     (16#6#, 16#7d#, 16#32#, 16#0d#),
     (16#6#, 16#60#, 16#3c#, 16#03#),
     (16#6#, 16#7f#, 16#34#, 16#0b#),
     (16#6#, 16#7f#, 16#3f#, 16#00#));

   TGL_Buffer_Trans_DP_HBR2_EDP_HBR3 : constant Buffer_Trans_DP_Array :=
      Buffer_Trans_DP_Array'(
     (16#a#, 16#35#, 16#3f#, 16#00#),
     (16#a#, 16#4f#, 16#37#, 16#08#),
     (16#c#, 16#71#, 16#2f#, 16#10#),
     (16#6#, 16#7f#, 16#2b#, 16#14#),
     (16#a#, 16#4c#, 16#3f#, 16#00#),
     (16#c#, 16#73#, 16#34#, 16#0b#),
     (16#6#, 16#7f#, 16#2f#, 16#10#),
     (16#c#, 16#6c#, 16#3c#, 16#03#),
     (16#6#, 16#7f#, 16#35#, 16#0a#),
     (16#6#, 16#7f#, 16#3f#, 16#00#));

   TGL_Buffer_Trans_EDP_HBR2 : constant Buffer_Trans_DP_Array :=
      Buffer_Trans_DP_Array'(
     (16#0#, 16#7F#, 16#3F#, 16#00#),
     (16#8#, 16#7F#, 16#38#, 16#07#),
     (16#1#, 16#7F#, 16#33#, 16#0C#),
     (16#9#, 16#7F#, 16#31#, 16#0E#),
     (16#8#, 16#7F#, 16#3F#, 16#00#),
     (16#1#, 16#7F#, 16#38#, 16#07#),
     (16#9#, 16#7F#, 16#35#, 16#0A#),
     (16#1#, 16#7F#, 16#3F#, 16#00#),
     (16#9#, 16#7F#, 16#38#, 16#07#),
     (16#9#, 16#7F#, 16#3F#, 16#00#));

   ADL_Buffer_Trans_EDP_HBR3 : constant Buffer_Trans_DP_Array :=
      Buffer_Trans_DP_Array'(
     (16#a#, 16#35#, 16#3f#, 16#00#),
     (16#a#, 16#4f#, 16#37#, 16#08#),
     (16#c#, 16#71#, 16#30#, 16#0f#),
     (16#6#, 16#7f#, 16#2b#, 16#14#),
     (16#a#, 16#4c#, 16#3f#, 16#00#),
     (16#c#, 16#73#, 16#34#, 16#0b#),
     (16#6#, 16#7f#, 16#30#, 16#0f#),
     (16#c#, 16#63#, 16#3f#, 16#00#),
     (16#6#, 16#7f#, 16#38#, 16#07#),
     (16#6#, 16#7f#, 16#3f#, 16#00#));

   ADL_Buffer_Trans_EDP_HBR2 : constant Buffer_Trans_DP_Array :=
      Buffer_Trans_DP_Array'(
     (16#4#, 16#50#, 16#38#, 16#07#),
     (16#4#, 16#58#, 16#35#, 16#0a#),
     (16#4#, 16#60#, 16#34#, 16#0b#),
     (16#4#, 16#6a#, 16#32#, 16#0d#),
     (16#4#, 16#5e#, 16#38#, 16#07#),
     (16#4#, 16#61#, 16#36#, 16#09#),
     (16#4#, 16#6b#, 16#34#, 16#0b#),
     (16#4#, 16#69#, 16#39#, 16#06#),
     (16#4#, 16#73#, 16#37#, 16#08#),
     (16#4#, 16#7a#, 16#38#, 16#07#));

   ADL_Buffer_Trans_DP_HBR3 : constant Buffer_Trans_DP_Array :=
      Buffer_Trans_DP_Array'(
     (16#a#, 16#35#, 16#3f#, 16#00#),
     (16#a#, 16#4f#, 16#37#, 16#08#),
     (16#c#, 16#71#, 16#30#, 16#0f#),
     (16#6#, 16#7f#, 16#2b#, 16#14#),
     (16#a#, 16#4c#, 16#3f#, 16#00#),
     (16#c#, 16#73#, 16#34#, 16#0b#),
     (16#6#, 16#7f#, 16#30#, 16#0f#),
     (16#c#, 16#63#, 16#3f#, 16#00#),
     (16#6#, 16#7f#, 16#38#, 16#07#),
     (16#6#, 16#7f#, 16#3f#, 16#00#));

   ADL_Buffer_Trans_DP_HBR : constant Buffer_Trans_DP_Array :=
      Buffer_Trans_DP_Array'(
     (16#a#, 16#35#, 16#3f#, 16#00#),
     (16#a#, 16#4f#, 16#37#, 16#08#),
     (16#c#, 16#71#, 16#31#, 16#0e#),
     (16#6#, 16#7f#, 16#2c#, 16#13#),
     (16#a#, 16#4c#, 16#3f#, 16#00#),
     (16#c#, 16#73#, 16#34#, 16#0b#),
     (16#6#, 16#7f#, 16#2f#, 16#10#),
     (16#c#, 16#7c#, 16#3c#, 16#03#),
     (16#6#, 16#7f#, 16#35#, 16#0a#),
     (16#6#, 16#7f#, 16#3f#, 16#00#));

   PORT_CL_DW10_PWR_DOWN_LN_MASK  : constant := 16#f# * 2 ** 4;
   PORT_CL_DW10_PWR_UP_ALL        : constant :=     0 * 2 ** 4;
   PORT_CL_DW10_PWR_DOWN_LN_3_2   : constant := 16#c# * 2 ** 4;
   PORT_CL_DW10_PWR_DOWN_LN_3_2_1 : constant := 16#e# * 2 ** 4;
   PORT_CL_DW10_PWR_DOWN_LN_1_0   : constant := 16#3# * 2 ** 4;
   PORT_CL_DW10_PWR_DOWN_LN_2_1_0 : constant := 16#7# * 2 ** 4;

   EDP4K2K_MODE_OVRD_EN        : constant := 1 * 2 **  2;
   EDP4K2K_MODE_OVRD_OPTIMIZED : constant := 1 * 2 **  3;

   ---------------------------------------------------------------------

   procedure Set_Vswing_And_Deemphasis
     (Port        : Combo_Port;
      Buf_Trans   : Buffer_Trans;
      Display     : Display_Type;
      Lane_Count  : Lanes_Range)
   is
      type Training_Values is (Training_Enable, Training_Disable);

      PORT_TX_DW5_TX_TRAINING_EN        : constant := 1 * 2 ** 31;
      PORT_TX_DW5_SCALING_MODE_SEL_MASK : constant := 7 * 2 ** 18;
      PORT_TX_DW5_RTERM_SELECT_MASK     : constant := 7 * 2 ** 3;
      PORT_TX_DW5_TAP2_DISABLE          : constant := 1 * 2 ** 30;
      PORT_TX_DW5_TAP3_DISABLE          : constant := 1 * 2 ** 29;
      PORT_TX_DW5_CURSOR_PROGRAM        : constant := 1 * 2 ** 26;
      PORT_TX_DW5_COEFF_POLARITY        : constant := 1 * 2 ** 25;

      PORT_TX_DW2_RCOMP_SCALAR_MASK     : constant := 16#ff# * 2 ** 0;
      PORT_TX_DW2_SWING_SEL_LOWER_MASK  : constant :=      7 * 2 ** 11;
      PORT_TX_DW2_SWING_SEL_UPPER       : constant :=      1 * 2 ** 15;

      PORT_TX_DW4_LOADGEN_SELECT        : constant :=      1 * 2 ** 31;
      PORT_TX_DW4_POST_CURSOR2_MASK     : constant := 16#3f# * 2 ** 6;
      PORT_TX_DW4_POST_CURSOR1_MASK     : constant := 16#3f# * 2 ** 12;
      PORT_TX_DW4_CURSOR_COEFF_MASK     : constant := 16#3f# * 2 ** 0;

      PORT_TX_DW7_N_SCALAR_MASK         : constant := 16#7f# * 2 ** 24;

      type Scaling_Mode is new Natural range 0 .. 7;
      function PORT_TX_DW5_SCALING_MODE_SEL (S : Scaling_Mode) return Word32 is
         (Shift_Left (Word32 (S), 18));

      type Rterm_Select is new Natural range 0 .. 7;
      function PORT_TX_DW5_RTERM_SELECT (R : Rterm_Select) return Word32 is
         (Shift_Left (Word32 (R), 3));

      type Rcomp_Scalar is new Natural range 0 .. 255;
      function PORT_TX_DW2_RCOMP_SCALAR (R : Rcomp_Scalar) return Word32 is
         (Word32 (R));

      procedure Set_Tx_Training (Port : Combo_Port; Training : Training_Values) is
         DW5 : Word32;
      begin
        Registers.Read (Port_Regs (Port).PORT_TX_DW5_LN0, DW5);
        Registers.Write
          (Register => Port_Regs (Port).PORT_TX_DW5_GRP,
           Value    => (if Training = Training_Enable
                        then DW5 or PORT_TX_DW5_TX_TRAINING_EN
                        else DW5 and not PORT_TX_DW5_TX_TRAINING_EN));
      end Set_Tx_Training;

      Tmp : Word32;
      PORT_CL_DW5_SUS_CLOCK_CONFIG : constant := 3 * 2 ** 0;
   begin
      -- Enable common keeper for DP/eDP only
      Registers.Read (Port_Regs (Port).PORT_PCS_DW1_LN0, Tmp);
      Registers.Write
        (Port_Regs (Port).PORT_PCS_DW1_GRP,
         (if Display = DP
          then Tmp or PORT_PCS_DW1_LN0_COMMON_KEEPER
          else Tmp and not PORT_PCS_DW1_LN0_COMMON_KEEPER));

      -- Program loadgen select (group access is not allowed since each lane may
      -- have a unique value.
      -- <= 6GHz, 4 lanes   (0, 1, 1, 1)
      -- <= 6GHz, 1,2 lanes (0, 1, 1, 0)
      -- > 6GHz,  any lanes (0, 0, 0, 0)
      for Lane in Lanes loop
         if (Lane_Count = 4 and Lane /= LN0) or
            (Lane_Count < 4 and (Lane = LN1 or Lane = LN2))
         then
            Registers.Set_Mask (PORT_TX_DW4 (Lane, Port),
                                PORT_TX_DW4_LOADGEN_SELECT);
         else
            Registers.Unset_Mask (PORT_TX_DW4 (Lane, Port),
                                  PORT_TX_DW4_LOADGEN_SELECT);
         end if;
      end loop;

      Registers.Set_Mask
        (Register => Port_Regs (Port).PORT_CL_DW5,
         Mask     => PORT_CL_DW5_SUS_CLOCK_CONFIG);

      -- In order to change swing values, training must be disabled
      Set_Tx_Training (Port, Training_Disable);

      if Display = DP then
         Registers.Unset_Mask
          (Register => Port_Regs (Port).PORT_CL_DW10,
           Mask     => EDP4K2K_MODE_OVRD_EN or EDP4K2K_MODE_OVRD_OPTIMIZED);
      end if;

      Registers.Read (Port_Regs (Port).PORT_TX_DW5_LN0, Tmp);
      Tmp := Tmp and not
         (PORT_TX_DW5_SCALING_MODE_SEL_MASK or
          PORT_TX_DW5_RTERM_SELECT_MASK or
          PORT_TX_DW5_TAP2_DISABLE or
          PORT_TX_DW5_TAP3_DISABLE or
          PORT_TX_DW5_CURSOR_PROGRAM or
          PORT_TX_DW5_COEFF_POLARITY);
      Tmp := Tmp or
         PORT_TX_DW5_SCALING_MODE_SEL (2) or
         PORT_TX_DW5_RTERM_SELECT (6) or
         PORT_TX_DW5_TAP3_DISABLE;
      Registers.Write (Port_Regs (Port).PORT_TX_DW5_GRP, Tmp);

      Registers.Read (Port_Regs (Port).PORT_TX_DW2_LN0, Tmp);
      Tmp := Tmp and not
        (PORT_TX_DW2_RCOMP_SCALAR_MASK or
         PORT_TX_DW2_SWING_SEL_LOWER_MASK or
         PORT_TX_DW2_SWING_SEL_UPPER);
      Tmp := Tmp or PORT_TX_SWING_SEL_UPPER (Buf_Trans.DW2_SWING_SEL);
      Tmp := Tmp or PORT_TX_SWING_SEL_LOWER (Buf_Trans.DW2_SWING_SEL);
      Tmp := Tmp or PORT_TX_DW2_RCOMP_SCALAR (16#98#);
      Registers.Write (Port_Regs (Port).PORT_TX_DW2_GRP, Tmp);

      -- Cannot write to GRP, because it would overwrite individual loadgen bits
      for Lane in Lanes loop
         Registers.Read (PORT_TX_DW4 (Lane, Port), Tmp);
         Tmp := Tmp and not
            (PORT_TX_DW4_POST_CURSOR2_MASK or
             PORT_TX_DW4_POST_CURSOR1_MASK or
             PORT_TX_DW4_CURSOR_COEFF_MASK);
         Tmp := Tmp or PORT_TX_DW4_POST_CURSOR1 (Buf_Trans.DW4_POST_CURSOR1);
         Tmp := Tmp or PORT_TX_DW4_CURSOR_COEFF (Buf_Trans.DW4_CURSOR_COEFF);
         Registers.Write (PORT_TX_DW4 (Lane, Port), Tmp);
      end loop;

      Registers.Read (Port_Regs (Port).PORT_TX_DW7_LN0, Tmp);
      Tmp := Tmp and not PORT_TX_DW7_N_SCALAR_MASK;
      Tmp := Tmp or PORT_TX_DW7_N_SCALAR (Buf_Trans.DW7_N_SCALAR);
      Registers.Write (Port_Regs (Port).PORT_TX_DW7_GRP, Tmp);

      -- To trigger an update of swing values, set training enable
      Set_Tx_Training (Port, Training_Enable);
   end Set_Vswing_And_Deemphasis;

   ---------------------------------------------------------------------

   procedure Power_Up_Lanes
     (Port  : Combo_Port;
      Lane_Count : Lanes_Range)
   is
      Lane_Mask : constant Word32 :=
        (case Lane_Count is
            -- if Lane_Reversal then PWR_DOWN_LN_2_1_0
            -- else PORT_CL_DW10_PWR_DOWN_LN_3_2_1
            when 1      => PORT_CL_DW10_PWR_DOWN_LN_3_2_1,
            -- if Lane_Reversal then PWR_DOWN_1_0
            -- else PORT_CL_DW10_PWR_DOWN_LN_3_2
            when 2      => PORT_CL_DW10_PWR_DOWN_LN_3_2,
            when others => PORT_CL_DW10_PWR_UP_ALL);
   begin
      Registers.Unset_And_Set_Mask
        (Register   => Port_Regs (Port).PORT_CL_DW10,
         Mask_Unset => PORT_CL_DW10_PWR_DOWN_LN_MASK,
         Mask_Set   => Lane_Mask);
   end Power_Up_Lanes;

   ---------------------------------------------------------------------

   procedure Set_Signal_Levels
     (Port        : Combo_Port;
      eDP         : Boolean;
      Link        : DP_Link;
      Train_Set   : DP_Info.Train_Set)
   is
      function Get_Buf_Trans_Table
        (eDP : Boolean) return Buffer_Trans_DP_Array is
      begin
         if Config.Has_TGL_Buffer_Translations then
            if eDP then
               if Link.Bandwidth > DP_Bandwidth_5_4 then
                  return TGL_Buffer_Trans_DP_HBR2_EDP_HBR3;
               else
                  return TGL_Buffer_Trans_DP_HBR;
               end if;
            else
               if Link.Bandwidth > DP_Bandwidth_2_7 then
                  if Config.Is_LP then
                     return TGL_Buffer_Trans_DP_HBR2_U_Y;
                 else
                     return TGL_Buffer_Trans_DP_HBR2;
                  end if;
               else
                  return TGL_Buffer_Trans_DP_HBR;
               end if;
            end if;
         else
            if eDP then
               if Link.Bandwidth > DP_Bandwidth_5_4 then
                  return ADL_Buffer_Trans_EDP_HBR3;
               else
                  return ADL_Buffer_Trans_DP_HBR; -- EDP_HBR2 ?
               end if;
            else
	       if Link.Bandwidth > DP_Bandwidth_2_7 then
	          return ADL_Buffer_Trans_DP_HBR3;
               else
                  return ADL_Buffer_Trans_DP_HBR;
	       end if;
	    end if;
         end if;
      end Get_Buf_Trans_Table;

      function To_Buf_Trans_Index
         (Set : DP_Info.Train_Set) return Buffer_Trans_DP_Range
      is
      begin
         case Set.Voltage_Swing is
         when DP_Info.VS_Level_0 =>
            case Set.Pre_Emph is
            when DP_Info.Emph_Level_0 => return 0;
            when DP_Info.Emph_Level_1 => return 1;
            when DP_Info.Emph_Level_2 => return 2;
            when DP_Info.Emph_Level_3 => return 3;
            end case;
         when DP_Info.VS_Level_1 =>
            case Set.Pre_Emph is
            when DP_Info.Emph_Level_0 => return 4;
            when DP_Info.Emph_Level_1 => return 5;
            when DP_Info.Emph_Level_2 => return 6;
            when others =>               return 0;
            end case;
         when DP_Info.VS_Level_2 =>
            case Set.Pre_Emph is
            when DP_Info.Emph_Level_0 => return 7;
            when DP_Info.Emph_Level_1 => return 8;
            when others =>               return 0;
            end case;
         when DP_Info.VS_Level_3 =>
            case Set.Pre_Emph is
            when DP_Info.Emph_Level_0 => return 9;
            when others =>               return 0;
            end case;
         end case;
      end To_Buf_Trans_Index;

      Was_Enabled : Boolean;
      Buf_Trans : Buffer_Trans;
      Entry_Index : constant Buffer_Trans_DP_Range :=
         To_Buf_Trans_Index (Train_Set);
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Registers.Is_Set_Mask
        (Register => Port_Regs (Port).DDI_BUF_CTL,
         Mask     => DDI_BUF_CTL_BUFFER_ENABLE,
         Result   => Was_Enabled);

      Buf_Trans := Get_Buf_Trans_Table (eDP) (Entry_Index);

      Set_Vswing_And_Deemphasis
        (Port,
         Buf_Trans,
         DP,
         Lanes_Range (Lane_Count_As_Integer (Link.Lane_Count)));

      if not Was_Enabled then
         Power_Up_Lanes (Port, Lanes_Range (Lane_Count_As_Integer (Link.Lane_Count)));
      end if;

      Registers.Unset_And_Set_Mask
        (Register    => Port_Regs (Port).DDI_BUF_CTL,
         Mask_Unset  => DDI_BUF_CTL_TRANS_SELECT_MASK or
                        DDI_BUF_CTL_PORT_REVERSAL or
                        DDI_BUF_CTL_PORT_WIDTH_MASK,
         Mask_Set    => DDI_BUF_CTL_BUFFER_ENABLE or
                        DDI_BUF_CTL_PORT_WIDTH (Link.Lane_Count));
      Registers.Posting_Read (Port_Regs (Port).DDI_BUF_CTL);

      if not Was_Enabled then
         Registers.Wait_Unset_Mask (Port_Regs (Port).DDI_BUF_CTL, DDI_BUF_CTL_IDLE_STATUS);
      end if;
   end Set_Signal_Levels;

   ---------------------------------------------------------------------

   procedure Enable_HDMI (Port : Combo_Port)
   is
      HDMI_Lane_Count : constant := 4;
   begin
      Set_Vswing_And_Deemphasis
        (Port,
         Buffer_Trans_HDMI (Buffer_Trans_HDMI'First),
         HDMI,
         HDMI_Lane_Count);

      Power_Up_Lanes (Port, HDMI_Lane_Count);

      Registers.Unset_And_Set_Mask
           (Register    => DDI_BUF_CTL (Port),
            Mask_Unset  => DDI_BUF_CTL_TRANS_SELECT_MASK or
                           DDI_BUF_CTL_PORT_REVERSAL or
                           DDI_BUF_CTL_PORT_WIDTH_MASK,
            Mask_Set    => DDI_BUF_CTL_BUFFER_ENABLE);

      Registers.Wait_Unset_Mask
        (Register => DDI_BUF_CTL (Port),
         Mask     => DDI_BUF_CTL_IDLE_STATUS);
   end Enable_HDMI;

end HW.GFX.GMA.Connectors.Combo_Phy;
