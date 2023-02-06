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
with HW.GFX.DP_Info;
with HW.GFX.GMA.PCode;

with HW.Debug;
with GNAT.Source_Info;

use type HW.Word64;

package body HW.GFX.GMA.Connectors.TC is

   function HIP_INDEX_REG (P : USBC_Port) return Registers.Registers_Index
   is (if P <= DDI_TC4
       then Registers.HIP_INDEX_REG0
       else Registers.HIP_INDEX_REG1);

   function HIP_INDEX_VAL (P : USBC_Port; Val : Word32) return Word32 is
     (case P is
      when DDI_TC1 | DDI_TC3 | DDI_TC5 => Val * 2 ** 0,
      when DDI_TC2 | DDI_TC4 | DDI_TC6 => Val * 2 ** 8);

   type Port_Regs_Array is array (USBC_Port) of Registers.Registers_Index;
   DKL_DP_MODE : constant Port_Regs_Array :=
      Port_Regs_Array'
     (DDI_TC1 => Registers.DKL_DP_MODE_1,
      DDI_TC2 => Registers.DKL_DP_MODE_2,
      DDI_TC3 => Registers.DKL_DP_MODE_3,
      DDI_TC4 => Registers.DKL_DP_MODE_4,
      DDI_TC5 => Registers.DKL_DP_MODE_5,
      DDI_TC6 => Registers.DKL_DP_MODE_6);

   function DP_PIN_ASSIGNMENT_SHIFT (P : USBC_Port) return natural is
     (case P is
      when DDI_TC1 => 0,
      when DDI_TC2 => 4,
      when DDI_TC3 => 8,
      when DDI_TC4 => 12,
      when DDI_TC5 => 16,
      when DDI_TC6 => 20);

   DP_PIN_ASSIGNMENT_MASK : constant := 16#f#;

   TGL_PCODE_TCCOLD         : constant := 16#26#;
   TCCOLD_BLOCK_REQ         : constant := 16#00#;
   TCCOLD_UNBLOCK_REQ       : constant := 16#01#;
   TCCOLD_BLOCK_RESULT_FAIL : constant := 16#01#;

   type Fia_Regs_Record is record
      PORT_TX_DFLEXDPMLE1 : Registers.Registers_Index;
      PORT_TX_DFLEXDPSP   : Registers.Registers_Index;
      PORT_TX_DFLEXDPPMS  : Registers.Registers_Index;
      PORT_TX_DFLEXDPCSSS : Registers.Registers_Index;
      PORT_TX_DFLEXPA1    : Registers.Registers_Index;
   end record;

   type Fia_Regs_Array is array (USBC_Port) of Fia_Regs_Record;
   Fia_Regs : constant Fia_Regs_Array :=
      Fia_Regs_Array'
     (DDI_TC1 =>
        (Registers.PORT_TX_DFLEXDPMLE1_FIA1,
         Registers.PORT_TX_DFLEXDPSP_FIA1,
         Registers.PORT_TX_DFLEXDPPMS_FIA1,
         Registers.PORT_TX_DFLEXDPCSSS_FIA1,
         Registers.PORT_TX_DFLEXPA1_FIA1),
      DDI_TC2 =>
        (Registers.PORT_TX_DFLEXDPMLE1_FIA1,
         Registers.PORT_TX_DFLEXDPSP_FIA1,
         Registers.PORT_TX_DFLEXDPPMS_FIA1,
         Registers.PORT_TX_DFLEXDPCSSS_FIA1,
         Registers.PORT_TX_DFLEXPA1_FIA1),
      DDI_TC3 =>
        (Registers.PORT_TX_DFLEXDPMLE1_FIA2,
         Registers.PORT_TX_DFLEXDPSP_FIA2,
         Registers.PORT_TX_DFLEXDPPMS_FIA2,
         Registers.PORT_TX_DFLEXDPCSSS_FIA2,
         Registers.PORT_TX_DFLEXPA1_FIA2),
      DDI_TC4 =>
        (Registers.PORT_TX_DFLEXDPMLE1_FIA2,
         Registers.PORT_TX_DFLEXDPSP_FIA2,
         Registers.PORT_TX_DFLEXDPPMS_FIA2,
         Registers.PORT_TX_DFLEXDPCSSS_FIA2,
         Registers.PORT_TX_DFLEXPA1_FIA2),
      DDI_TC5 =>
        (Registers.PORT_TX_DFLEXDPMLE1_FIA3,
         Registers.PORT_TX_DFLEXDPSP_FIA3,
         Registers.PORT_TX_DFLEXDPPMS_FIA3,
         Registers.PORT_TX_DFLEXDPCSSS_FIA3,
         Registers.PORT_TX_DFLEXPA1_FIA3),
      DDI_TC6 =>
        (Registers.PORT_TX_DFLEXDPMLE1_FIA3,
         Registers.PORT_TX_DFLEXDPSP_FIA3,
         Registers.PORT_TX_DFLEXDPPMS_FIA3,
         Registers.PORT_TX_DFLEXDPCSSS_FIA3,
         Registers.PORT_TX_DFLEXPA1_FIA3));

   function Fia_Index (Port : USBC_Port) return natural
   is (case Port is
       when DDI_TC1 | DDI_TC3 | DDI_TC5 => 0,
       when DDI_TC2 | DDI_TC4 | DDI_TC6 => 1);

   function DFLEXDPMLE1_DPMLETC_MASK (Port : USBC_Port) return Word32 is
      (Shift_Left (15, 4 * Fia_Index (Port)));
   function DFLEXDPMLE1_DPMLETC_ML0 (Port : USBC_Port) return Word32 is
      (Shift_Left (1, 4 * Fia_Index (Port)));
   function DFLEXDPMLE1_DPMLETC_ML1_0 (Port : USBC_Port) return Word32 is
      (Shift_Left (3, 4 * Fia_Index (Port)));
   function DFLEXDPMLE1_DPMLETC_ML3 (Port : USBC_Port) return Word32 is
      (Shift_Left (8, 4 * Fia_Index (Port)));
   function DFLEXDPMLE1_DPMLETC_ML3_2 (Port : USBC_Port) return Word32 is
      (Shift_Left (12, 4 * Fia_Index (Port)));
   function DFLEXDPMLE1_DPMLETC_ML3_0 (Port : USBC_Port) return Word32 is
      (Shift_Left (15, 4 * Fia_Index (Port)));
   function DP_PHY_MODE_STATUS_COMPLETE (Port : USBC_Port) return Word32 is
      (Shift_Left (1, Fia_Index (Port)));
   function DP_PHY_MODE_STATUS_NOT_SAFE (Port : USBC_Port) return Word32 is
      (Shift_Left (1, Fia_Index (Port)));
   function TC_LIVE_STATE_TC (Port : USBC_Port) return Word32 is
      (Shift_Left (1, Fia_Index (Port) * 8 + 5));
   function DP_LANE_ASSIGNMENT_MASK (Port : USBC_Port) return Word32 is
      (Shift_Left (16#f#, Fia_Index (Port) * 8));
   function DP_LANE_ASSIGNMENT_SHIFT (Port : USBC_Port) return natural is
      (Fia_Index (Port) * 8);

   DDI_BUF_CTL_BUFFER_ENABLE        : constant :=      1 * 2 ** 31;
   DDI_BUF_CTL_TRANS_SELECT_MASK    : constant :=  16#f# * 2 ** 24;
   DDI_BUF_CTL_PORT_REVERSAL        : constant :=      1 * 2 ** 16;
   DDI_BUF_CTL_PORT_WIDTH_MASK      : constant :=      7 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_1_LANE    : constant :=      0 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_2_LANES   : constant :=      1 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_4_LANES   : constant :=      3 * 2 **  1;
   DDI_BUF_CTL_IDLE_STATUS          : constant :=      1 * 2 **  7;

   type DDI_BUF_CTL_PORT_WIDTH_T is array (HW.GFX.DP_Lane_Count) of Word32;
   DDI_BUF_CTL_PORT_WIDTH : constant DDI_BUF_CTL_PORT_WIDTH_T :=
      DDI_BUF_CTL_PORT_WIDTH_T'
     (HW.GFX.DP_Lane_Count_1 => DDI_BUF_CTL_PORT_WIDTH_1_LANE,
      HW.GFX.DP_Lane_Count_2 => DDI_BUF_CTL_PORT_WIDTH_2_LANES,
      HW.GFX.DP_Lane_Count_4 => DDI_BUF_CTL_PORT_WIDTH_4_LANES);
   DDI_BUF_CTL : constant Port_Regs_Array :=
      Port_Regs_Array'
     (DDI_TC1  => Registers.DDI_BUF_CTL_USBC1,
      DDI_TC2  => Registers.DDI_BUF_CTL_USBC2,
      DDI_TC3  => Registers.DDI_BUF_CTL_USBC3,
      DDI_TC4  => Registers.DDI_BUF_CTL_USBC4,
      DDI_TC5  => Registers.DDI_BUF_CTL_USBC5,
      DDI_TC6  => Registers.DDI_BUF_CTL_USBC6);

   type Buffer_Trans is record
      Vswing_Control     : Word32;
      Preshoot_Control   : Word32;
      Deemphasis_Control : Word32;
   end record;

   type Buffer_Trans_Range is new natural range 0 .. 9;
   type Buffer_Trans_Array is array (Buffer_Trans_Range) of Buffer_Trans;
   TGL_Buffer_Trans_DP_HBR2 : constant Buffer_Trans_Array :=
      Buffer_Trans_Array'(
     (16#7#, 16#0#, 16#00#),
     (16#5#, 16#0#, 16#05#),
     (16#2#, 16#0#, 16#0B#),
     (16#0#, 16#0#, 16#19#),
     (16#5#, 16#0#, 16#00#),
     (16#2#, 16#0#, 16#08#),
     (16#0#, 16#0#, 16#14#),
     (16#2#, 16#0#, 16#00#),
     (16#0#, 16#0#, 16#0B#),
     (16#0#, 16#0#, 16#00#));
   TGL_Buffer_Trans_DP_HBR : constant Buffer_Trans_Array :=
      Buffer_Trans_Array'(
     (16#7#, 16#0#, 16#00#),
     (16#5#, 16#0#, 16#05#),
     (16#2#, 16#0#, 16#0B#),
     (16#0#, 16#0#, 16#18#),
     (16#5#, 16#0#, 16#00#),
     (16#2#, 16#0#, 16#08#),
     (16#0#, 16#0#, 16#14#),
     (16#2#, 16#0#, 16#00#),
     (16#0#, 16#0#, 16#0B#),
     (16#0#, 16#0#, 16#00#));
   Buffer_Trans_HDMI : constant Buffer_Trans_Array :=
      Buffer_Trans_Array'(
     (16#7#, 16#0#, 16#0#),
     (16#6#, 16#0#, 16#0#),
     (16#4#, 16#0#, 16#0#),
     (16#2#, 16#0#, 16#0#),
     (16#0#, 16#0#, 16#0#),
     (16#0#, 16#0#, 16#5#),
     (16#0#, 16#0#, 16#6#),
     (16#0#, 16#0#, 16#7#),
     (16#0#, 16#0#, 16#8#),
     (16#0#, 16#0#, 16#A#));
   ADL_Buffer_Trans_DP_HBR2 : constant Buffer_Trans_Array :=
     Buffer_Trans_Array'(
     (16#7#, 16#0#, 16#00#),
     (16#5#, 16#0#, 16#04#),
     (16#2#, 16#0#, 16#0a#),
     (16#0#, 16#0#, 16#18#),
     (16#5#, 16#0#, 16#00#),
     (16#2#, 16#0#, 16#06#),
     (16#0#, 16#0#, 16#14#),
     (16#2#, 16#0#, 16#00#),
     (16#0#, 16#0#, 16#09#),
     (16#0#, 16#0#, 16#00#));
   ADL_Buffer_Trans_DP_HBR : constant Buffer_Trans_Array :=
     Buffer_Trans_Array'(
     (16#7#, 16#0#, 16#01#),
     (16#5#, 16#0#, 16#06#),
     (16#2#, 16#0#, 16#0b#),
     (16#0#, 16#0#, 16#17#),
     (16#5#, 16#0#, 16#00#),
     (16#2#, 16#0#, 16#08#),
     (16#0#, 16#0#, 16#14#),
     (16#2#, 16#0#, 16#00#),
     (16#0#, 16#0#, 16#0b#),
     (16#0#, 16#0#, 16#00#));

   type Vswing_Regs_Record is record
      DKL_TX_PMD_LANE_SUS  : Registers.Registers_Index;
      DKL_TX_DPCNTL0       : Registers.Registers_Index;
      DKL_TX_DPCNTL1       : Registers.Registers_Index;
      DKL_TX_DPCNTL2       : Registers.Registers_Index;
   end record;

   type Vswing_Regs_Array is array (USBC_Port) of Vswing_Regs_Record;
   Vswing_Regs : constant Vswing_Regs_Array :=
      Vswing_Regs_Array'
     (DDI_TC1 =>
        (Registers.DKL_TX_PMD_LANE_SUS_1,
         Registers.DKL_TX_DPCNTL0_1,
         Registers.DKL_TX_DPCNTL1_1,
         Registers.DKL_TX_DPCNTL2_1),
      DDI_TC2 =>
        (Registers.DKL_TX_PMD_LANE_SUS_2,
         Registers.DKL_TX_DPCNTL0_2,
         Registers.DKL_TX_DPCNTL1_2,
         Registers.DKL_TX_DPCNTL2_2),
      DDI_TC3 =>
        (Registers.DKL_TX_PMD_LANE_SUS_3,
         Registers.DKL_TX_DPCNTL0_3,
         Registers.DKL_TX_DPCNTL1_3,
         Registers.DKL_TX_DPCNTL2_3),
      DDI_TC4 =>
        (Registers.DKL_TX_PMD_LANE_SUS_4,
         Registers.DKL_TX_DPCNTL0_4,
         Registers.DKL_TX_DPCNTL1_4,
         Registers.DKL_TX_DPCNTL2_4),
      DDI_TC5 =>
        (Registers.DKL_TX_PMD_LANE_SUS_5,
         Registers.DKL_TX_DPCNTL0_5,
         Registers.DKL_TX_DPCNTL1_5,
         Registers.DKL_TX_DPCNTL2_5),
      DDI_TC6 =>
        (Registers.DKL_TX_PMD_LANE_SUS_6,
         Registers.DKL_TX_DPCNTL0_6,
         Registers.DKL_TX_DPCNTL1_6,
         Registers.DKL_TX_DPCNTL2_6));

   procedure Set_HIP_For_Port (P : USBC_Port; N : Natural) is
   begin
      Registers.Write (HIP_INDEX_REG (P), HIP_INDEX_VAL (P, Word32 (N)));
   end Set_HIP_For_Port;

   subtype Pin_Assignment_Type is natural range 0 .. 6;
   procedure Get_Pin_Assignment
     (P          : in    USBC_Port;
      Assignment : out   Pin_Assignment_Type)
   is
      Tmp : Word32;
      A : Word32;
   begin
      Registers.Read (Fia_Regs (P).PORT_TX_DFLEXPA1, Tmp);
      A := Shift_Right (Tmp, DP_PIN_ASSIGNMENT_SHIFT (P));
      A := A and DP_PIN_ASSIGNMENT_MASK;

      if natural (A) in Pin_Assignment_Type then
         Assignment := Pin_Assignment_Type (A);
      else
         Assignment := Pin_Assignment_Type'First;
      end if;
   end Get_Pin_Assignment;

   ---------------------------------------------------------------------

   procedure Program_DP_Mode (P : USBC_Port; Lane_Count : Natural)
   is
      MG_DP_MODE_CFG_DP_X1_MODE : constant := 1 * 2 ** 6;
      MG_DP_MODE_CFG_DP_X2_MODE : constant := 1 * 2 ** 7;
      DP_X_MODE_MASK : constant Word32 :=
         MG_DP_MODE_CFG_DP_X1_MODE or MG_DP_MODE_CFG_DP_X2_MODE;
      Assignment : Pin_Assignment_Type;
      Ln0, Ln1 : Word32;
   begin
      Set_HIP_For_Port (P, 0);
      Registers.Read (DKL_DP_MODE (P), Ln0);
      Set_HIP_For_Port (P, 1);
      Registers.Read (DKL_DP_MODE (P), Ln1);

      Ln0 := Ln0 and not DP_X_MODE_MASK;
      Ln1 := Ln1 and not DP_X_MODE_MASK;

      Get_Pin_Assignment (P, Assignment);
      case Assignment is
      when 0 =>
         if Lane_Count = 1 then
            Ln1 := Ln1 or MG_DP_MODE_CFG_DP_X1_MODE;
         else
            Ln0 := Ln0 or MG_DP_MODE_CFG_DP_X2_MODE;
            Ln1 := Ln1 or MG_DP_MODE_CFG_DP_X2_MODE;
         end if;
      when 1 =>
         if Lane_Count = 4 then
            Ln0 := Ln0 or MG_DP_MODE_CFG_DP_X2_MODE;
            Ln1 := Ln1 or MG_DP_MODE_CFG_DP_X2_MODE;
         end if;
      when 2 =>
         if Lane_Count = 2 then
            Ln0 := Ln0 or MG_DP_MODE_CFG_DP_X2_MODE;
            Ln1 := Ln1 or MG_DP_MODE_CFG_DP_X2_MODE;
         end if;
      when 3 | 4 | 5 | 6 =>
         if Lane_Count = 1 then
            Ln0 := Ln0 or MG_DP_MODE_CFG_DP_X1_MODE;
            Ln1 := Ln1 or MG_DP_MODE_CFG_DP_X1_MODE;
         else
            Ln0 := Ln0 or MG_DP_MODE_CFG_DP_X2_MODE;
            Ln1 := Ln1 or MG_DP_MODE_CFG_DP_X2_MODE;
         end if;
      end case;

      Set_HIP_For_Port (P, 0);
      Registers.Unset_And_Set_Mask (DKL_DP_MODE (P), DP_X_MODE_MASK, Ln0);
      Set_HIP_For_Port (P, 1);
      Registers.Unset_And_Set_Mask (DKL_DP_MODE (P), DP_X_MODE_MASK, Ln1);
   end Program_DP_Mode;

   ---------------------------------------------------------------------

   procedure TC_Cold_Request
     (Request : in     TC_Cold_Request_Type;
      Success :    out Boolean)
   is
      Result : Word64;
   begin
      for Try in 1 .. 3 loop
         PCode.Mailbox_Read
           (MBox       => TGL_PCODE_TCCOLD,
            Command    => (if Request = Block
                           then TCCOLD_BLOCK_REQ
                           else TCCOLD_UNBLOCK_REQ),
            Wait_Ready => True,
            Reply      => Result,
            Success    => Success);

         if Success then
            Success := (Result and TCCOLD_BLOCK_RESULT_FAIL) = 0;
         end if;

         exit when Success;

         -- Wait 1 millisecond and try again
         Time.U_Delay (1_000);
      end loop;
   end TC_Cold_Request;

   ---------------------------------------------------------------------

   procedure Set_Lane_Count (Port : USBC_Port; Lanes : Natural) is
   begin
      Registers.Unset_And_Set_Mask
        (Register   => Fia_Regs (Port).PORT_TX_DFLEXDPMLE1,
         Mask_Unset => DFLEXDPMLE1_DPMLETC_MASK (Port),
         Mask_Set   => (case Lanes is
                        -- ML0 is not lane-reversed, ML3 is reverse
                        when 1 => DFLEXDPMLE1_DPMLETC_ML0 (Port),
                        -- ML1_0 is not reversed, ML3_2 is reverse
                        when 2 => DFLEXDPMLE1_DPMLETC_ML1_0 (Port),
                        -- symmetric
                        when 4 => DFLEXDPMLE1_DPMLETC_ML3_0 (Port),
                        when others => 0));
   end Set_Lane_Count;

   ---------------------------------------------------------------------

   procedure Is_DP_Phy_Mode_Status_Complete
     (Port : USBC_Port;
      Success: out Boolean)
   is
   begin
      Registers.Is_Set_Mask
        (Register => Fia_Regs (Port).PORT_TX_DFLEXDPPMS,
         Mask     => DP_PHY_MODE_STATUS_COMPLETE (Port),
         Result   => Success);
   end Is_DP_Phy_Mode_Status_Complete;

   procedure Connect
     (Port    : in     USBC_Port;
      Success :    out Boolean)
   is
      procedure Get_Lane_Assignment_Count
        (Port : USBC_Port;
         Lanes: out natural)
      is
          Lane_Mask : Word32;
          Tmp : Word32;
      begin
         Registers.Read (Fia_Regs (Port).PORT_TX_DFLEXDPSP, Tmp);
         Lane_Mask :=
            Shift_Right (Tmp and DP_LANE_ASSIGNMENT_MASK (Port),
                         DP_LANE_ASSIGNMENT_SHIFT (Port));
         Lanes :=
           (case Lane_Mask is
            when 1 | 2 | 4 | 8 => 1,
            when 3 | 12        => 2,
            when 15            => 4,
            when others        => 1);
         end Get_Lane_Assignment_Count;
      Lanes : natural;
   begin
      Is_DP_Phy_Mode_Status_Complete (Port, Success);
      if not Success then
         Debug.Put_Line ("DP PHY mode status not complete");
         return;
      end if;

      Registers.Set_Mask
        (Register => Fia_Regs (Port).PORT_TX_DFLEXDPCSSS,
         Mask     => DP_PHY_MODE_STATUS_NOT_SAFE (Port));

      Registers.Is_Set_Mask
        (Register => Fia_Regs (Port).PORT_TX_DFLEXDPSP,
         Mask     => TC_LIVE_STATE_TC (Port),
         Result   => Success);

      if not Success then
         pragma Debug (Debug.Put_Line ("Type-C Port is not connected."));
      end if;

      pragma Debug
        (not Success, Debug.Put_Line ("Type-C Port is not connected."));

      Get_Lane_Assignment_Count (Port, Lanes);
      Set_Lane_Count (Port, Lanes);
   end Connect;

   ---------------------------------------------------------------------

   procedure Disconnect (Port : USBC_Port) is
   begin
      Registers.Unset_Mask
        (Register => Fia_Regs (Port).PORT_TX_DFLEXDPCSSS,
         Mask     => Word32 (Fia_Index (Port)));

      Registers.Wait_Unset_mask
        (Register => Fia_Regs (Port).PORT_TX_DFLEXDPPMS,
         Mask     => DP_PHY_MODE_STATUS_COMPLETE (Port));
   end Disconnect;

   ---------------------------------------------------------------------

   procedure Set_Vswing_And_Deemphasis
      (Port      : USBC_Port;
       Buf_Trans : Buffer_Trans)
   is
      -- Preshoot Coeff, Deemphasis Coeff, VSwing Control,
      DPcnt_Mask : constant Word32 := 16#3_ff07#;
      DPcnt_Val : constant Word32 :=
         Buf_Trans.Vswing_Control or
         Shift_Left (Buf_Trans.Deemphasis_Control, 8) or
         Shift_Left (Buf_Trans.Preshoot_Control, 13);
      DKL_TX_DP20BITMODE : constant := 1 * 2 ** 2;
   begin
      for Lane in 0 .. 1 loop
         Set_HIP_For_Port (Port, Lane);
         Registers.Write (Vswing_Regs (Port).DKL_TX_PMD_LANE_SUS, 0);
         Registers.Unset_And_Set_Mask
            (Vswing_Regs (Port).DKL_TX_DPCNTL0, DPcnt_Mask, DPcnt_Val);
         Registers.Unset_And_Set_Mask
            (Vswing_Regs (Port).DKL_TX_DPCNTL1, DPcnt_Mask, DPcnt_Val);
         Registers.Unset_Mask
            (Vswing_Regs (Port).DKL_TX_DPCNTL2, DKL_TX_DP20BITMODE);
      end loop;

   end Set_Vswing_And_Deemphasis;

   ---------------------------------------------------------------------

   procedure Set_Signal_Levels
     (Port        : USBC_Port;
      Link        : DP_Link;
      Train_Set   : DP_Info.Train_Set)
   is
      function To_Buf_Trans_Index
         (Set : DP_Info.Train_Set) return Buffer_Trans_Range
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
      Entry_Index : constant Buffer_Trans_Range :=
         To_Buf_Trans_Index (Train_Set);
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Registers.Is_Set_Mask
        (Register => DDI_BUF_CTL (Port),
         Mask     => DDI_BUF_CTL_BUFFER_ENABLE,
         Result   => Was_Enabled);

      if Config.Has_TGL_Buffer_Translations then
         if Link.Bandwidth > DP_Bandwidth_2_7 then
            Buf_Trans := TGL_Buffer_Trans_DP_HBR2 (Entry_Index);
         else
            Buf_Trans := TGL_Buffer_Trans_DP_HBR (Entry_Index);
         end if;
      else
         if Link.Bandwidth > DP_Bandwidth_2_7 then
            Buf_Trans := ADL_Buffer_Trans_DP_HBR2 (Entry_Index);
         else
            Buf_Trans := ADL_Buffer_Trans_DP_HBR (Entry_Index);
         end if;
      end if;

      Set_Vswing_And_Deemphasis (Port, Buf_Trans);

      Registers.Unset_And_Set_Mask
        (Register    => DDI_BUF_CTL (Port),
         Mask_Unset  => DDI_BUF_CTL_TRANS_SELECT_MASK or
                        DDI_BUF_CTL_PORT_REVERSAL or
                        DDI_BUF_CTL_PORT_WIDTH_MASK,
         Mask_Set    => DDI_BUF_CTL_BUFFER_ENABLE or
                        DDI_BUF_CTL_PORT_WIDTH (Link.Lane_Count));
      Registers.Posting_Read (DDI_BUF_CTL (Port));

      if not Was_Enabled then
         Registers.Wait_Unset_Mask (DDI_BUF_CTL (Port), DDI_BUF_CTL_IDLE_STATUS);
      end if;
   end Set_Signal_Levels;

   procedure Enable_HDMI (Port : USBC_Port)
   is
      HDMI_Lane_Count : constant := 4;
      Buf_Trans : constant Buffer_Trans :=
         Buffer_Trans_HDMI (Buffer_Trans_HDMI'Last);
   begin
      Program_DP_Mode (Port, HDMI_Lane_Count);
      Set_Vswing_And_Deemphasis (Port, Buf_Trans);
   end Enable_HDMI;

end HW.GFX.GMA.Connectors.TC;
