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
with HW.GFX.GMA.DP_Info;
with HW.GFX.GMA.Registers;

package HW.GFX.GMA.Connectors.TC is

   procedure Program_DP_Mode (P : USBC_Port; Lane_Count : Natural);
   procedure Enable_HDMI (Port : USBC_Port);

   procedure Set_Signal_Levels
     (Port        : USBC_Port;
      Link        : DP_Link;
      Train_Set   : DP_Info.Train_Set);

   type TC_Cold_Request_Type is (Block, Unblock);
   procedure TC_Cold_Request
     (Request : in     TC_Cold_Request_Type;
      Success :    out Boolean);

private
   -- Some of the Connectors code only supports 4 ports.
   -- And so far, no SKU needs more.
   subtype Valid_TC_Port is USBC_Port range DDI_TC1 .. Config.Last_TC_Port;

   type Port_Regs_Array is
      array (USBC_Port) of Registers.Registers_Invalid_Index;

   DDI_BUF_CTL_BUFFER_ENABLE        : constant :=      1 * 2 ** 31;
   DDI_BUF_CTL_TRANS_SELECT_MASK    : constant :=  16#f# * 2 ** 24;
   DDI_BUF_CTL_PORT_REVERSAL        : constant :=      1 * 2 ** 16;
   DDI_BUF_CTL_PORT_WIDTH_MASK      : constant :=      7 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_1_LANE    : constant :=      0 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_2_LANES   : constant :=      1 * 2 **  1;
   DDI_BUF_CTL_PORT_WIDTH_4_LANES   : constant :=      3 * 2 **  1;
   DDI_BUF_CTL_IDLE_STATUS          : constant :=      1 * 2 **  7;
   DDI_BUF_CTL_TC_PHY_OWNERSHIP     : constant :=      1 * 2 **  6;

   DDI_BUF_CTL_PORT_WIDTH : constant array (DP_Lane_Count) of Word32 :=
     (HW.GFX.DP_Lane_Count_1 => DDI_BUF_CTL_PORT_WIDTH_1_LANE,
      HW.GFX.DP_Lane_Count_2 => DDI_BUF_CTL_PORT_WIDTH_2_LANES,
      HW.GFX.DP_Lane_Count_4 => DDI_BUF_CTL_PORT_WIDTH_4_LANES);
   DDI_BUF_CTL : constant Port_Regs_Array :=
     (DDI_TC1  => Registers.DDI_BUF_CTL_USBC1,
      DDI_TC2  => Registers.DDI_BUF_CTL_USBC2,
      DDI_TC3  => Registers.DDI_BUF_CTL_USBC3,
      DDI_TC4  => Registers.DDI_BUF_CTL_USBC4,
      DDI_TC5  => Registers.DDI_BUF_CTL_USBC5,
      DDI_TC6  => Registers.DDI_BUF_CTL_USBC6);

   type Fia_Regs_Record is record
      PORT_TX_DFLEXDPMLE1 : Registers.Registers_Index;
      PORT_TX_DFLEXDPSP   : Registers.Registers_Index;
      PORT_TX_DFLEXDPPMS  : Registers.Registers_Index;
      PORT_TX_DFLEXDPCSSS : Registers.Registers_Index;
      PORT_TX_DFLEXPA1    : Registers.Registers_Index;
   end record;

   type Fia_Regs_Array is array (USBC_Port) of Fia_Regs_Record;
   Fia_Regs : constant Fia_Regs_Array :=
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

   function Fia_Index (Port : USBC_Port) return Natural
   is
     (case Port is
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
   function DP_LANE_ASSIGNMENT_SHIFT (Port : USBC_Port) return Natural is
      (Fia_Index (Port) * 8);

   procedure Get_Lane_Assignment_Count
     (Port  : in     USBC_Port;
      Lanes :    out DP_Lane_Count);
   procedure Set_Lane_Count (Port : USBC_Port; Lanes : DP_Lane_Count);

end HW.GFX.GMA.Connectors.TC;
