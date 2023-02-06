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

with GNAT.Source_Info;
with HW.Debug;
with HW.GFX.GMA.Combo_Phy;
with HW.GFX.GMA.Config;
with HW.GFX.GMA.PCode;
with HW.GFX.GMA.Registers;
with HW.GFX.GMA.Transcoder;
with HW.GFX.GMA.Connectors.TC;

use type HW.Word64;

package body HW.GFX.GMA.Power_And_Clocks is

   subtype CDClk_Range is Config.CDClk_Range;

   type Power_Domain is
     (PW1, PW2, PW3, PW4, PW5,
      DDI_A, DDI_B, DDI_C,
      DDI_USBC1, DDI_USBC2, DDI_USBC3, DDI_USBC4, DDI_USBC5, DDI_USBC6,
      AUX_A, AUX_B, AUX_C,
      AUX_USBC1, AUX_USBC2, AUX_USBC3, AUX_USBC4, AUX_USBC5, AUX_USBC6);
   type Power_Domain_Types is (Power_Well, Power_DDI, Power_AUX);
   subtype Dynamic_Domain  is Power_Domain range PW2 .. Power_Domain'Last;
   subtype PW_Domain       is Power_Domain range PW1 .. PW5;
   subtype Dynamic_Well    is Power_Domain range PW2 .. PW_Domain'Last;
   subtype Port_Domain     is Power_Domain range DDI_A .. AUX_USBC6;
   subtype DDI_Domain      is Power_Domain range DDI_A .. DDI_USBC6;
   subtype DDI_USBC_Domain is Power_Domain range DDI_USBC1 .. DDI_USBC6;
   subtype AUX_Domain      is Power_Domain range AUX_A .. AUX_USBC6;
   subtype AUX_USBC_Domain is Power_Domain range AUX_USBC1 .. AUX_USBC6;

   function PW_Index (PW : PW_Domain) return Natural
   is
     (Power_Domain'Pos (PW) - Power_Domain'Pos (PW_Domain'First));

   function DDI_Index (DDI : DDI_Domain) return Natural
   is
     (Power_Domain'Pos (DDI) - Power_Domain'Pos (DDI_Domain'First));

   function AUX_Index (AUX : AUX_Domain) return Natural
   is
     (Power_Domain'Pos (AUX) - Power_Domain'Pos (AUX_Domain'First));

   function AUX_USBC_Index (AUX: AUX_USBC_Domain) return  Natural
   is
     (Power_Domain'Pos (AUX) - Power_Domain'Pos (AUX_USBC_Domain'First));

   ----------------------------------------------------------------------------

   function Power_Domain_Type (PD : Power_Domain) return Power_Domain_Types
   is
     (if    PD in PW_Domain  then Power_Well
      elsif PD in DDI_Domain then Power_DDI
      else                        Power_AUX);

   type Power_Well_Regs is array (Power_Domain_Types) of Registers.Registers_Index;
   PWR_CTL_BIOS : constant Power_Well_Regs :=
     (Power_Well => Registers.PWR_WELL_CTL_BIOS,
      Power_DDI  => Registers.PWR_DDI_CTL_BIOS,
      Power_AUX  => Registers.PWR_AUX_CTL_BIOS);
   PWR_CTL_DRIVER : constant Power_Well_Regs :=
     (Power_Well => Registers.PWR_WELL_CTL_DRIVER,
      Power_DDI  => Registers.PWR_DDI_CTL_DRIVER,
      Power_AUX  => Registers.PWR_AUX_CTL_DRIVER);

   function Power_State_Mask (PD : Power_Domain) return Word32
   is
     (case PD is
         when PW_Domain'Range    => 1 * 2 ** (2 * PW_Index (PD)),
         when DDI_Domain'Range   => 1 * 2 ** (2 * DDI_Index (PD)),
         when AUX_Domain'Range   => 1 * 2 ** (2 * AUX_Index (PD)));

   function Power_Request_Mask (PD : Power_Domain) return Word32 is
   begin
      return Shift_Left (Power_State_Mask (PD), 1);
   end Power_Request_Mask;

   ----------------------------------------------------------------------------

   PCH_DPMGUNIT_CLOCK_GATE_DISABLE                : constant := 1 * 2 ** 15;
   NDE_RSTWRN_OPT_RST_PCH_Handshake_En            : constant := 1 * 2 ** 4;

   ----------------------------------------------------------------------------

   DBUF_CTL_DBUF_POWER_REQUEST                    : constant := 1 * 2 ** 31;
   DBUF_CTL_TRACKER_STATE_SERVICE_MASK            : constant := 16#f8_0000#;
   DBUF_CTL_TRACKER_STATE_SERVICE_SHIFT           : constant := 19;
   DBUF_CTL_DBUF_POWER_STATE                      : constant := 1 * 2 ** 30;

   type DBUF_Slices is (S1, S2);
   DBUF_CTL : constant array (DBUF_Slices) of Registers.Registers_Index :=
     (Registers.DBUF_CTL_S0,
      Registers.DBUF_CTL_S1);

   ----------------------------------------------------------------------------

   MBUS_ABOX_CTL_BW_CREDITS_MASK                  : constant := 16#3#  * 2 ** 20;
   MBUS_ABOX_CTL_B_CREDITS_MASK                   : constant := 16#f#  * 2 ** 16;
   MBUS_ABOX_CTL_BT_CREDITS_POOL1_MASK            : constant := 16#1f# * 2 **  0;
   MBUS_ABOX_CTL_BT_CREDITS_POOL2_MASK            : constant := 16#1f# * 2 **  8;
   MBUS_ABOX_CTL_BW_CREDITS_SHIFT                 : constant := 20;
   MBUS_ABOX_CTL_B_CREDITS_SHIFT                  : constant := 16;
   MBUS_ABOX_CTL_BT_CREDITS_POOL1_SHIFT           : constant := 0;
   MBUS_ABOX_CTL_BT_CREDITS_POOL2_SHIFT           : constant := 8;

   MBUS_ABOX_MASK : constant Word32 :=
     (MBUS_ABOX_CTL_BW_CREDITS_MASK or
      MBUS_ABOX_CTL_B_CREDITS_MASK or
      MBUS_ABOX_CTL_BT_CREDITS_POOL1_MASK or
      MBUS_ABOX_CTL_BT_CREDITS_POOL2_MASK);
   MBUS_ABOX_CREDITS : constant Word32 :=
     ( 1 * 2 ** MBUS_ABOX_CTL_BW_CREDITS_SHIFT or
       1 * 2 ** MBUS_ABOX_CTL_B_CREDITS_SHIFT or
      16 * 2 ** MBUS_ABOX_CTL_BT_CREDITS_POOL1_SHIFT or
      16 * 2 ** MBUS_ABOX_CTL_BT_CREDITS_POOL2_SHIFT);

   MBUS_ABOX_CTL : constant array (0 .. 2) of Registers.Registers_Index :=
     (Registers.MBUS_ABOX_CTL,
      Registers.MBUS_ABOX1_CTL,
      Registers.MBUS_ABOX2_CTL);

   ----------------------------------------------------------------------------

   DCPR_MASK_MAXLATENCY_MEMUP_CLR                 : constant := 1 * 2 ** 27;
   DCPR_MASK_LPMODE                               : constant := 1 * 2 ** 26;
   DCPR_SEND_RESP_IMM                             : constant := 1 * 2 ** 25;
   DCPR_CLEAR_MEMSTAT_DIS                         : constant := 1 * 2 ** 24;

   ----------------------------------------------------------------------------

   function HIP_INDEX_REG (Aux : AUX_USBC_Domain) return Registers.Registers_Index
   is
     (if Aux <= AUX_USBC4
      then Registers.HIP_INDEX_REG0
      else Registers.HIP_INDEX_REG1);

   function HIP_INDEX_VAL (Aux : AUX_USBC_Domain; Val : Word32) return Word32
   is
     (Val * 2 ** (8 * (AUX_USBC_Index (Aux) mod 4)));

   type DKL_Regs is array (AUX_USBC_Domain) of Registers.Registers_Index;
   DKL_CMN_UC_DW_27 : constant DKL_Regs :=
     (AUX_USBC1 => Registers.DKL_CMN_UC_DW_27_1,
      AUX_USBC2 => Registers.DKL_CMN_UC_DW_27_2,
      AUX_USBC3 => Registers.DKL_CMN_UC_DW_27_3,
      AUX_USBC4 => Registers.DKL_CMN_UC_DW_27_4,
      AUX_USBC5 => Registers.DKL_CMN_UC_DW_27_5,
      AUX_USBC6 => Registers.DKL_CMN_UC_DW_27_6);

   ----------------------------------------------------------------------------

   FUSE_STATUS_PG0_DIST_STATUS : constant := 1 * 2 ** 27;
   FUSE_STATUS_PGx_DIST_STATUS : constant array (PW_Domain) of Word32 :=
     (PW1   => 1 * 2 ** 26,
      PW2   => 1 * 2 ** 25,
      PW3   => 1 * 2 ** 24,
      PW4   => 1 * 2 ** 23,
      PW5   => 1 * 2 ** 22);

   ----------------------------------------------------------------------------

   TGL_PCODE_MEM_SUBSYSTEM_INFO                   : constant := 16#d#;
   TGL_PCODE_MEM_SS_READ_GLOBAL_INFO              : constant := 0 * 2 ** 8;
   TGL_PCODE_CDCLK_CONTROL                        : constant := 7;
   TGL_CDCLK_PREPARE_FOR_CHANGE                   : constant := 3;
   TGL_CDCLK_READY_FOR_CHANGE                     : constant := 1;

   ----------------------------------------------------------------------------

   CDCLK_PLL_ENABLE_PLL_RATIO_MASK                : constant := 16#ff#;
   CDCLK_PLL_ENABLE_PLL_ENABLE                    : constant := 1 * 2 ** 31;
   CDCLK_PLL_ENABLE_PLL_LOCK                      : constant := 1 * 2 ** 30;
   CDCLK_CD2X_DIV_SEL_MASK                        : constant := 3 * 2 ** 22;
   CDCLK_CD2X_DIV_SEL_1                           : constant := 0 * 2 ** 22;
   CDCLK_CD2X_DIV_SEL_2                           : constant := 2 * 2 ** 22;
   CDCLK_CD2X_PIPE_NONE                           : constant := 7 * 2 ** 19;
   CDCLK_CTL_CD_FREQ_DECIMAL_MASK                 : constant := 16#7ff#;

   ----------------------------------------------------------------------------

   type AUX_CTL_Array is array (AUX_USBC_Domain) of Registers.Registers_Index;
   AUX_CTL_Regs : constant AUX_CTL_Array :=
     (AUX_USBC1 => Registers.DDI_AUX_CTL_USBC1,
      AUX_USBC2 => Registers.DDI_AUX_CTL_USBC2,
      AUX_USBC3 => Registers.DDI_AUX_CTL_USBC3,
      AUX_USBC4 => Registers.DDI_AUX_CTL_USBC4,
      AUX_USBC5 => Registers.DDI_AUX_CTL_USBC5,
      AUX_USBC6 => Registers.DDI_AUX_CTL_USBC6);

   ----------------------------------------------------------------------------

   function To_GPU_Port (PD : Port_Domain) return GPU_Port
   is
     (case PD is
         when DDI_A | AUX_A         => DIGI_A,
         when DDI_B | AUX_B         => DIGI_B,
         when DDI_C | AUX_C         => DIGI_C,
         when DDI_USBC1 | AUX_USBC1 => DDI_TC1,
         when DDI_USBC2 | AUX_USBC2 => DDI_TC2,
         when DDI_USBC3 | AUX_USBC3 => DDI_TC3,
         when DDI_USBC4 | AUX_USBC4 => DDI_TC4,
         when DDI_USBC5 | AUX_USBC5 => DDI_TC5,
         when DDI_USBC6 | AUX_USBC6 => DDI_TC6);

   procedure Pre_PD_On (PD : in Power_Domain; Success : out Boolean)
   is
      DP_AUX_CH_CTL_TBT_IO : constant := 1 * 2 ** 11;
   begin
      if PD in AUX_USBC_Domain then
         -- Disable TBT IO mode for AUX
         Registers.Unset_Mask
           (Register => AUX_CTL_Regs (PD),
            Mask     => DP_AUX_CH_CTL_TBT_IO);
         Connectors.TC.Claimed (To_GPU_Port (PD), Success);
      elsif PD = PW1 then
         Registers.Wait_Set_Mask
           (Register => Registers.FUSE_STATUS,
            Mask     => FUSE_STATUS_PG0_DIST_STATUS,
            Success  => Success);
      else
         Success := True;
      end if;
   end Pre_PD_On;

   procedure Post_PD_On (PD : Power_Domain)
   is
      DKL_CMN_UC_DW_27_UC_HEALTH : constant := 1 * 2 ** 15;
   begin
      if PD in PW_Domain then
         Registers.Wait_Set_Mask
           (Register => Registers.FUSE_STATUS,
            Mask     => FUSE_STATUS_PGx_DIST_STATUS (PD),
            TOut_MS  => 1);
      elsif PD in AUX_USBC_Domain then
         Registers.Write (HIP_INDEX_REG (PD), HIP_INDEX_VAL (PD, 2));
         Registers.Wait_Set_Mask
           (Register => DKL_CMN_UC_DW_27 (PD),
            Mask     => DKL_CMN_UC_DW_27_UC_HEALTH,
            TOut_MS  => 1);
      end if;
   end Post_PD_On;

   procedure Pre_PD_Off (PD : Power_Domain) is
   begin
      if PD in DDI_USBC_Domain then
         -- Could be moved to a higher level, but right now it's
         -- convenient to do it here: When requested to turn the
         -- power off, we know exactly that we don't want to use
         -- the port (anymore).
         Connectors.TC.Disconnect (To_GPU_Port (PD));
      end if;
   end Pre_PD_Off;

   procedure PD_On (PD : Power_Domain)
   is
      Ctl1, Ctl2 : Word32;
      PD_Type : constant Power_Domain_Types := Power_Domain_Type (PD);
      Success : Boolean;
   begin
      Registers.Read (PWR_CTL_BIOS (PD_Type), Ctl1);
      Registers.Read (PWR_CTL_DRIVER (PD_Type), Ctl2);

      if ((Ctl1 or Ctl2) and Power_Request_Mask (PD)) = 0 then
         Registers.Wait_Unset_Mask
           (Register => PWR_CTL_DRIVER (PD_Type),
            Mask     => Power_State_Mask (PD),
            TOut_MS  => 1);
      end if;

      if (Ctl2 and Power_Request_Mask (PD)) = 0 then
         Pre_PD_On (PD, Success);
         if not Success then
            pragma Debug (Debug.Put_Line ("Connection flow failed!"));
            return;
         end if;

         Registers.Set_Mask (PWR_CTL_DRIVER (PD_Type), Power_Request_Mask (PD));

         Registers.Wait_Set_Mask
           (Register => PWR_CTL_DRIVER (PD_Type),
            Mask     => Power_State_Mask (PD),
            TOut_MS  => 1,
            Success  => Success);
         pragma Debug (not Success, Debug.Put_Line ("Failed to enable power domain!"));

         if Success then
            Post_PD_On (PD);
         end if;
      end if;
   end PD_On;

   procedure PD_Off (PD : Power_Domain)
   is
      Ctl1, Ctl2 : Word32;
      PD_Type : constant Power_Domain_Types := Power_Domain_Type (PD);
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Registers.Read (PWR_CTL_BIOS (PD_Type), Ctl1);
      Registers.Read (PWR_CTL_DRIVER (PD_Type), Ctl2);

      if ((Ctl1 or Ctl2) and Power_Request_Mask (PD)) /= 0 then
         Registers.Wait_Set_Mask
           (Register => PWR_CTL_DRIVER (PD_Type),
            Mask     => Power_State_Mask (PD),
            TOut_MS  => 1);

         Pre_PD_Off (PD);

         Registers.Unset_Mask (PWR_CTL_DRIVER (PD_Type), Power_Request_Mask (PD));
         Registers.Unset_Mask (PWR_CTL_BIOS (PD_Type), Power_Request_Mask (PD));
      end if;
   end PD_Off;

   function Need_PW (PW : Dynamic_Well; Configs : Pipe_Configs) return Boolean
   is
      function Any_TC_Port return Boolean is
        (for some Pipe in Pipe_Index =>
            Configs (Pipe).Port /= Disabled and then
            Config_Helpers.To_GPU_Port (Pipe, Configs (Pipe).Port) in USBC_Port);

      function Any_Pipe_From (First : Pipe_Index) return Boolean is
        (for some Pipe in First .. Pipe_Index'Last => Configs (Pipe).Port /= Disabled);

      function VGA return Boolean is
        (Configs (Primary).Framebuffer.Offset = VGA_PLANE_FRAMEBUFFER_OFFSET);
   begin
      case PW is
         when PW2 | PW3 =>
            return Any_Pipe_From (Secondary) or Any_TC_Port or VGA;
         when PW4 =>
            return Any_Pipe_From (Tertiary);
         when PW5 =>
            return False;  -- Fourth pipe not supported yet.
      end case;
   end Need_PW;

   function Need_PD (PD : Dynamic_Domain; Configs : Pipe_Configs) return Boolean
   is
      function Any_Port_Is (Port : GPU_Port) return Boolean is
        (for some Pipe in Pipe_Index =>
            Configs (Pipe).Port /= Disabled and then
            Config_Helpers.To_GPU_Port (Pipe, Configs (Pipe).Port) = Port);
   begin
      return
        (case PD is
            when Dynamic_Well'Range => Need_PW (PD, Configs),
            when Port_Domain'Range => Any_Port_Is (To_GPU_Port (PD)));
   end Need_PD;

   procedure Get_RefClk (Refclk : out Refclk_Range)
   is
      DSSM : Word32;
      DSSM_REFERENCE_FREQUENCY_MASK    : constant := 16#e000_0000#;
      DSSM_REFERENCE_FREQUENCY_24MHZ   : constant := 16#0000_0000#;
      DSSM_REFERENCE_FREQUENCY_19_2MHZ : constant := 16#2000_0000#;
      DSSM_REFERENCE_FREQUENCY_38_4MHZ : constant := 16#4000_0000#;
   begin
      Registers.Read (Registers.DSSM, DSSM);
      Refclk :=
        (case DSSM and DSSM_REFERENCE_FREQUENCY_MASK is
         when DSSM_REFERENCE_FREQUENCY_24MHZ   => 24_000_000,
         when DSSM_REFERENCE_FREQUENCY_19_2MHZ => 19_200_000,
         when DSSM_REFERENCE_FREQUENCY_38_4MHZ => 38_400_000,
         when others                           => 24_000_000);
   end Get_Refclk;

   procedure Get_RawClk (Rawclk : out Frequency_Type)
   is
      Raw_Frequency_24_MHz : Boolean;
      SFUSE_STRAP_RAW_FREQUENCY : constant := 1 * 2 ** 8;
   begin
      Rawclk := Config.Default_RawClk_Freq;
      Registers.Is_Set_Mask
        (Register => Registers.SFUSE_STRAP,
         Mask     => SFUSE_STRAP_RAW_FREQUENCY,
         Result   => Raw_Frequency_24_MHz);

      if not Raw_Frequency_24_MHz then
         Rawclk := 19_200_000;
      end if;
   end Get_RawClk;

   procedure Get_Max_CDClk (CDClk : out CDClk_Range)
   is
      Refclk_Freq : Refclk_Range;
   begin
      Get_Refclk (Refclk_Freq);
      CDClk :=
        (case Refclk_Freq is
         when 24_000_000 => 648_000_000,
         when others     => 652_800_000);
   end Get_Max_CDClk;

   procedure Normalize_CDClk
     (CDClk       : in     Int64;
      Normalized  :    out CDClk_Range)
   with
      Post => Normalized >= 172_800_000
   is
      Refclk_Freq : Refclk_Range;
   begin
      Get_Refclk (Refclk_Freq);
      Normalized :=
        (case Refclk_Freq is
         when 19_200_000 | 38_400_000 =>
            (if    CDClk <= 172_800_000 then 172_800_000
             elsif CDClk <= 192_000_000 then 192_000_000
             elsif CDClk <= 307_200_000 then 307_200_000
             elsif CDClk <= 326_400_000 then 326_400_000
             elsif CDClk <= 556_800_000 then 556_800_000
                                        else 652_800_000),
         when others =>
            (if    CDClk <= 180_000_000 then 180_000_000
             elsif CDClk <= 192_000_000 then 192_000_000
             elsif CDClk <= 312_000_000 then 312_000_000
             elsif CDClk <= 324_000_000 then 324_000_000
             elsif CDClk <= 552_000_000 then 552_000_000
                                        else 648_000_000));
   end Normalize_CDClk;

   procedure Get_Cur_CDClk (CDClk : out CDClk_Range)
   with
      Post => CDClk >= 172_800_000
   is
      CDCLK_CTL : Word32;
   begin
      Registers.Read (Registers.CDCLK_CTL, CDCLK_CTL);
      CDCLK_CTL := CDCLK_CTL and CDCLK_CTL_CD_FREQ_DECIMAL_MASK;
      Normalize_CDClk (Int64 (CDCLK_CTL) * 500_000 + 1_000_000, CDClk);
   end Get_Cur_CDClk;

   procedure Set_CDClk (CDClk_In : CDClk_Range)
   is
      subtype PLL_Ratio_Range is Word32 range 0 .. 68;
      function Ratio_For_19_2_MHz (CDClk : CDClk_Range) return PLL_Ratio_Range is
      begin
         case CDClk is
            when 172_800_000 => return 18;
            when 192_000_000 => return 20;
            when 307_200_000 => return 32;
            when 326_400_000 | 652_800_000 => return 68;
            when 556_800_000 => return 58;
            when others => return 0;
         end case;
      end Ratio_For_19_2_MHz;

      function Ratio_For_24_MHz (CDCLk : CDClk_Range) return PLL_Ratio_Range is
      begin
         case CDClk is
            when 180_800_000 => return 15;
            when 192_000_000 => return 16;
            when 312_000_000 => return 26;
            when 324_000_000 | 648_000_000 => return 54;
            when 552_000_000 => return 46;
            when others => return 0;
         end case;
      end Ratio_For_24_MHz;

      function CDCLK_CTL_CD_FREQ_DECIMAL (Freq : CDClk_Range) return Word32
      with
         Pre => Freq > 1_000_000
      is
      begin
         -- Weirdest representation: CDClk - 1MHz in 10.1 (10 + 1 fractional bit)
         return Word32 (Div_Round_Closest (Pos64 (Freq) - 1_000_000, 500_000));
      end CDCLK_CTL_CD_FREQ_DECIMAL;

      Success : Boolean;
      CD2X : Word32;
      PLL_Ratio : PLL_Ratio_Range;
      CDClk : CDClk_Range;
      Refclk_Freq : Refclk_Range;
      VCO : Pos64;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Normalize_CDClk (CDClk_Range'Min (CDClk_In, Config.Max_CDClk), CDClk);
      Get_Refclk (Refclk_Freq);
      PLL_Ratio := (case Refclk_Freq is
         when 19_200_000 => Ratio_For_19_2_MHz (CDClk),
         when 38_400_000 => Ratio_For_19_2_MHz (CDClk) / 2,
         when 24_000_000 => Ratio_For_24_MHz (CDClk),
         when others     => 0);

      if PLL_Ratio = 0 then
         pragma Debug (Debug.Put_Line
                       ("ERROR: Invalid Refclk frequency, bad hardware?"));
         return;
      end if;

      PCode.Mailbox_Request
        (Mbox       => TGL_PCODE_CDCLK_CONTROL,
         Command    => TGL_CDCLK_PREPARE_FOR_CHANGE,
         Reply_Mask => TGL_CDCLK_READY_FOR_CHANGE,
         Wait_Ready => True,
         Success    => Success);

      if not Success then
         pragma Debug (Debug.Put_Line
                       ("ERROR: PCODE not ready for frequency change."));
         return;
      end if;

      Registers.Unset_Mask
        (Register => Registers.CDCLK_PLL_ENABLE,
         Mask     => CDCLK_PLL_ENABLE_PLL_ENABLE);
      Registers.Wait_Unset_Mask
        (Register => Registers.CDCLK_PLL_ENABLE,
         Mask     => CDCLK_PLL_ENABLE_PLL_LOCK);

      Registers.Write
        (Register => Registers.CDCLK_PLL_ENABLE,
         Value    => PLL_Ratio);
      Registers.Write
        (Register => Registers.CDCLK_PLL_ENABLE,
         Value    => PLL_Ratio or CDCLK_PLL_ENABLE_PLL_ENABLE);
      Registers.Wait_Set_Mask
        (Register => Registers.CDCLK_PLL_ENABLE,
         Mask     => CDCLK_PLL_ENABLE_PLL_LOCK,
         Success  => Success);

      if not Success then
         Debug.Put_Line ("CDClk PLL failed to lock!");
         return;
      end if;

      VCO := (Refclk_Freq / 1_000) * Pos64 (PLL_Ratio);
      CD2X :=
         (case (Div_Round_Closest (VCO, CDClk / 1_000)) is
          when 2 => CDCLK_CD2X_DIV_SEL_1,
          when 4 => CDCLK_CD2X_DIV_SEL_2,
          when others => CDCLK_CD2X_DIV_SEL_1);

      Registers.Write
        (Register => Registers.CDCLK_CTL,
         Value    => CDCLK_CTL_CD_FREQ_DECIMAL (CDClk) or
                     CDCLK_CD2X_PIPE_NONE or CD2X);

      PCode.Mailbox_Write
        (MBox     => TGL_PCODE_CDCLK_CONTROL,
         Command  => (if    CDClk <= 312_000_000 then 0
                      elsif CDClk <= 326_400_000 then 1
                      elsif CDClk <= 556_800_000 then 2
                      else 3));
      Config.CDClk := CDClk;

      pragma Debug (Debug.Put ("Set CDClk to "));
      pragma Debug (Debug.Put_Int64 (CDClk / 1_000_000));
      pragma Debug (Debug.Put ("."));
      pragma Debug (Debug.Put_Int64 ((CDClk mod 1_000_000) / 100_000));
      pragma Debug (Debug.Put_Line ("MHz."));
   end Set_CDClk;

   procedure Configure_Bandwidth_Buddy
   is
      BW_BUDDY_DISABLE : constant := 1 * 2 ** 31;
      BW_BUDDY_TLB_REQ_TIMER_MASK : constant := 16#3f_0000#;

      type DRAM_Module_Type is (DDR4, DDR5, LPDDR4, LPDDR5);
      type Bw_Buddy_Info is record
         DRAM_Channels : Natural;
         DRAM_Type     : DRAM_Module_Type;
         BW_BUDDY_MASK : Word32;
      end record;
      type Bw_Buddy_Info_Array is array (1 .. 8) of Bw_Buddy_Info;

      Buddy_Info : constant Bw_Buddy_Info_Array :=
        ((1, DDR4,   16#0f#),
         (1, DDR5,   16#0f#),
         (2, LPDDR4, 16#1c#),
         (2, LPDDR5, 16#1c#),
         (2, DDR4,   16#1f#),
         (2, DDR5,   16#1e#),
         (4, LPDDR4, 16#38#),
         (4, LPDDR5, 16#38#));

      -- TODO: use for ADL-S, RKL A0/B0
      Buddy_Info_Wa_1409767108 : constant Bw_Buddy_Info_Array :=
        ((1, DDR4,   1),
         (1, DDR5,   1),
         (1, LPDDR4, 1),
         (1, LPDDR5, 1),
         (2, DDR4,   3),
         (2, DDR5,   3),
         (2, LPDDR4, 3),
         (2, LPDDR5, 3));

      Result : Word64;
      Module_Type: DRAM_Module_Type;
      Channels : Natural;
      Success : Boolean;
   begin
      PCode.Mailbox_Read(MBox => TGL_PCODE_MEM_SUBSYSTEM_INFO or
                                 TGL_PCODE_MEM_SS_READ_GLOBAL_INFO,
                         Wait_Ready => True,
                         Reply => Result,
                         Success => Success);
      if not Success then
         pragma Debug (Debug.Put_Line
                         ("ERROR: PCODE didn't return memory info."));
         return;
      end if;

      case (Result and 16#f#) is
         when 0 => Module_Type := DDR4;
         when 1 => Module_Type := DDR5;
         when 2 => Module_Type := LPDDR5;
         when 3 => Module_Type := LPDDR4;
         when others =>
            pragma Debug (Debug.Put_Line ("ERROR: Invalid DRAM Module Type."));
            return;
      end case;

      Channels := Natural (Shift_Right (Result and 16#f0#, 4));
      for I in Buddy_Info'Range loop
         if Buddy_Info (I).DRAM_Type = Module_Type and
               Buddy_Info (I).DRAM_Channels = Channels
         then
            Registers.Set_Mask
              (Register => Registers.BW_BUDDY1_PAGE_MASK,
               Mask     => Buddy_Info (I).BW_BUDDY_MASK);
            Registers.Set_Mask
              (Register => Registers.BW_BUDDY2_PAGE_MASK,
               Mask     => Buddy_Info (I).BW_BUDDY_MASK);

            -- Wa_22010178259:tgl,rkl
            Registers.Unset_And_Set_Mask
              (Register   => Registers.BW_BUDDY1_CTL,
               Mask_Unset => BW_BUDDY_TLB_REQ_TIMER_MASK,
               Mask_Set   => 8 * 2 ** 16);
            Registers.Unset_And_Set_Mask
              (Register   => Registers.BW_BUDDY2_CTL,
               Mask_Unset => BW_BUDDY_TLB_REQ_TIMER_MASK,
               Mask_Set   => 8 * 2 ** 16);

            return;
         end if;
      end loop;

      Registers.Write (Registers.BW_BUDDY1_CTL, BW_BUDDY_DISABLE);
      Registers.Write (Registers.BW_BUDDY2_CTL, BW_BUDDY_DISABLE);
   end Configure_Bandwidth_Buddy;

   ----------------------------------------------------------------------------

   procedure Initialize
   is
      RawClk : Frequency_Type;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      -- Wa_14011294188:ehl,jsl,tgl,rkl,adl-s,adl-p
      Registers.Set_Mask
        (Register => Registers.PCH_DSPCLK_GATE_D,
         Mask     => PCH_DPMGUNIT_CLOCK_GATE_DISABLE);

      Registers.Set_Mask
        (Register => Registers.NDE_RSTWRN_OPT,
         Mask     => NDE_RSTWRN_OPT_RST_PCH_Handshake_En);

      PD_On (PW1);

      Get_Cur_CDClk (Config.CDClk);
      Get_Max_CDClk (Config.Max_CDClk);
      if Config.CDClk < Config.Default_CDClk_Freq then
         Set_CDClk (Config.Default_CDClk_Freq);
      end if;

      Get_RawClk (RawClk);
      Config.Raw_Clock := RawClk;

      -- TGL: Set DBUF Tracker State Service to 8
      Registers.Unset_And_Set_Mask
        (Register    => DBUF_CTL (S1),
         Mask_Unset  => DBUF_CTL_TRACKER_STATE_SERVICE_MASK,
         Mask_Set    => 8 * 2 ** DBUF_CTL_TRACKER_STATE_SERVICE_SHIFT);

      -- Enable first DBUF slice (TODO: Is this ok to use for all pipes?)
      Registers.Set_Mask (DBUF_CTL (S1), DBUF_CTL_DBUF_POWER_REQUEST);
      Registers.Wait_Set_Mask (DBUF_CTL (S1), DBUF_CTL_DBUF_POWER_STATE);

      for I in MBUS_ABOX_CTL'Range loop
         Registers.Unset_And_Set_Mask
           (Register    => MBUS_ABOX_CTL (I),
            Mask_Unset  => MBUS_ABOX_MASK,
            Mask_Set    => MBUS_ABOX_CREDITS);
      end loop;

      Configure_Bandwidth_Buddy;

      -- Display WA #14011508470 tgl,dg1,rkl,adl-s,adl-p,dg2
      Registers.Set_Mask
        (Register => Registers.GEN11_CHICKEN_DCPR_2,
         Mask     => DCPR_MASK_MAXLATENCY_MEMUP_CLR or DCPR_MASK_LPMODE or
                     DCPR_SEND_RESP_IMM or DCPR_CLEAR_MEMSTAT_DIS);
   end Initialize;

   procedure Limit_Dotclocks
     (Configs        : in out Pipe_Configs;
      CDClk_Switch   :    out Boolean)
   is
      CDClk : CDClk_Range;
   begin
      Config_Helpers.Limit_Dotclocks (Configs, Config.Max_CDClk);
      Normalize_CDClk (Config_Helpers.Highest_Dotclock (Configs), CDClk);
      CDClk_Switch := Config.CDClk /= CDClk;
   end Limit_Dotclocks;

   procedure Update_CDClk (Configs : in out Pipe_Configs)
   is
      New_CDClk : constant Frequency_Type :=
         Config_Helpers.Highest_Dotclock (Configs);
   begin
      Set_CDClk (New_CDClk);
      Config_Helpers.Limit_Dotclocks (Configs, Config.CDClk);
   end Update_CDClk;

   procedure Enable_CDClk is
   begin
      if Config.CDClk < Config.Default_CDClk_Freq then
         Set_CDClk (Config.Default_CDClk_Freq);
      end if;
   end Enable_CDClk;

   ----------------------------------------------------------------------------

   procedure Power_Set_To (Configs : Pipe_Configs) is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for PD in reverse Dynamic_Domain loop
         if not Need_PD (PD, Configs) then
            PD_Off (PD);
         end if;
      end loop;

      for PD in Dynamic_Domain loop
         if Need_PD (PD, Configs) then
            PD_On (PD);
         end if;
      end loop;
   end Power_Set_To;

   procedure Power_Up (Port : Active_Port_Type; Success : out Boolean)
   is
      GPU_Port : constant GMA.GPU_Port :=
         Config_Helpers.To_GPU_Port (Pipe_Index'First, Port);

      procedure On (Aux : AUX_Domain; DDI : DDI_Domain) is
      begin
         PD_On (PW1);
         if GPU_Port in USBC_Port then
            PD_On (PW2);
            PD_On (PW3);
         end if;
         PD_On (DDI);

         if GPU_Port in USBC_Port then
            Connectors.TC.Claim
              (Port     => GPU_Port,
               DP_Alt   => Port in Physical_USBC_Ports,
               Success  => Success);
            if not Success then
               return;
            end if;
         end if;
         PD_On (Aux);

         Success := True;
      end On;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      case GPU_Port is
         when GMA.DIGI_A   => On (AUX_A, DDI_A);
         when GMA.DIGI_B   => On (AUX_B, DDI_B);
         when GMA.DIGI_C   => On (AUX_C, DDI_C);
         when GMA.DDI_TC1  => On (AUX_USBC1, DDI_USBC1);
         when GMA.DDI_TC2  => On (AUX_USBC2, DDI_USBC2);
         when GMA.DDI_TC3  => On (AUX_USBC3, DDI_USBC3);
         when GMA.DDI_TC4  => On (AUX_USBC4, DDI_USBC4);
         when GMA.DDI_TC5  => On (AUX_USBC5, DDI_USBC5);
         when GMA.DDI_TC6  => On (AUX_USBC6, DDI_USBC6);
         when others       => Success := True;
      end case;
   end Power_Up;

   procedure Power_Up (Old_Configs, New_Configs : Pipe_Configs) is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      -- Power wells only, Aux/DDI domains are enabled later on explicit request.
      for PW in Dynamic_Well loop
         if not Need_PW (PW, Old_Configs) and Need_PW (PW, New_Configs) then
            PD_On (PW);
         end if;
      end loop;
   end Power_Up;

   procedure Power_Down (Old_Configs, Tmp_Configs, New_Configs : Pipe_Configs)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for PD in reverse Dynamic_Domain loop
        if (Need_PD (PD, Old_Configs) or Need_PD (PD, Tmp_Configs)) and
           not Need_PD (PD, New_Configs)
        then
           PD_Off (PD);
        end if;
      end loop;
   end Power_Down;

   procedure Pre_All_Off is
   begin
      Transcoder.PSR_Off;
   end Pre_All_Off;

   procedure Post_All_Off is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for S in reverse DBUF_CTL'Range loop
         Registers.Unset_Mask
            (DBUF_CTL (S), DBUF_CTL_DBUF_POWER_REQUEST);
         Registers.Wait_Unset_Mask
            (DBUF_CTL (S), DBUF_CTL_DBUF_POWER_STATE);
      end loop;

      -- Disable CDClk PLL. FIXME: Not implemented yet.
      Set_CDClk (CDClk_Range'First);

      for PD in reverse Power_Domain loop
         PD_Off (PD);
      end loop;

      Combo_Phy.All_Off;
   end Post_All_Off;

end HW.GFX.GMA.Power_And_Clocks;
