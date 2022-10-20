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
with HW.GFX.GMA.Power_And_Clocks.TGL;
with HW.GFX.GMA.Power_And_Clocks.XELPD;

use type HW.Word64;

package body HW.GFX.GMA.Power_And_Clocks is

   DARBF_GATING_DIS                               : constant := 1 * 2 ** 27;
   PCH_DPMGUNIT_CLOCK_GATE_DISABLE                : constant := 1 * 2 ** 15;
   NDE_RSTWRN_OPT_RST_PCH_Handshake_En            : constant := 1 * 2 ** 4;

   ----------------------------------------------------------------------------

   DBUF_CTL_DBUF_POWER_REQUEST                    : constant := 1 * 2 ** 31;
   DBUF_CTL_TRACKER_STATE_SERVICE_MASK            : constant := 16#f8_0000#;
   DBUF_CTL_TRACKER_STATE_SERVICE_SHIFT           : constant := 19;
   DBUF_CTL_MIN_TRACKER_STATE_SERVICE_SHIFT       : constant := 16;
   DBUF_CTL_MIN_TRACKER_STATE_SERVICE_MASK        : constant := 16#7_0000#;
   DBUF_CTL_DBUF_POWER_STATE                      : constant := 1 * 2 ** 30;

   type DBUF_Regs_Array is array (natural range 0 .. 3) of Registers.Registers_Index;
   DBUF_Regs : constant DBUF_Regs_Array :=
      DBUF_Regs_Array'
     (Registers.DBUF_CTL_S0,
      Registers.DBUF_CTL_S1,
      Registers.DBUF_CTL_S2,
      Registers.DBUF_CTL_S3);

   MBUS_JOIN                                      : constant := 1 * 2 ** 31;
   MBUS_HASHING_MODE_MASK                         : constant := 1 * 2 ** 30;
   MBUS_HASHING_MODE_2x2                          : constant := 0 * 2 ** 30;
   MBUS_HASHING_MODE_1x4                          : constant := 1 * 2 ** 30;
   MBUS_JOIN_PIPE_SELECT_MASK                     : constant := 16#1c00_0000#;
   MBUS_JOIN_PIPE_SELECT_NONE                     : constant := 16#1c00_0000#;

   ----------------------------------------------------------------------------

   MBUS_ABOX_CTL_BW_CREDITS_MASK                  : constant := 16#3#  * 2 ** 20;
   MBUS_ABOX_CTL_B_CREDITS_MASK                   : constant := 16#f#  * 2 ** 16;
   MBUS_ABOX_CTL_BT_CREDITS_POOL1_MASK            : constant := 16#1f# * 2 **  0;
   MBUS_ABOX_CTL_BT_CREDITS_POOL2_MASK            : constant := 16#1f# * 2 **  8;
   MBUS_ABOX_CTL_BW_CREDITS_SHIFT                 : constant := 20;
   MBUS_ABOX_CTL_B_CREDITS_SHIFT                  : constant := 16;
   MBUS_ABOX_CTL_BT_CREDITS_POOL1_SHIFT           : constant := 0;
   MBUS_ABOX_CTL_BT_CREDITS_POOL2_SHIFT           : constant := 8;

   MBUS_ABOX_MASK : constant := Word32'(
      MBUS_ABOX_CTL_BW_CREDITS_MASK or
      MBUS_ABOX_CTL_B_CREDITS_MASK or
      MBUS_ABOX_CTL_BT_CREDITS_POOL1_MASK or
      MBUS_ABOX_CTL_BT_CREDITS_POOL2_MASK);
   MBUS_ABOX_CREDITS : constant := Word32'(
      1 * 2 ** MBUS_ABOX_CTL_BW_CREDITS_SHIFT or
      1 * 2 ** MBUS_ABOX_CTL_B_CREDITS_SHIFT or
      16 * 2 ** MBUS_ABOX_CTL_BT_CREDITS_POOL1_SHIFT or
      16 * 2 ** MBUS_ABOX_CTL_BT_CREDITS_POOL2_SHIFT);

   type MBUSRegs is array (Natural range 0 .. 2) of Registers.Registers_Index;
   MBUS_ABOX_CTL : constant MBUSRegs := MBUSRegs'
     (Registers.MBUS_ABOX_CTL,
      Registers.MBUS_ABOX1_CTL,
      Registers.MBUS_ABOX2_CTL);

   ----------------------------------------------------------------------------

   DCPR_MASK_MAXLATENCY_MEMUP_CLR                 : constant := 1 * 2 ** 27;
   DCPR_MASK_LPMODE                               : constant := 1 * 2 ** 26;
   DCPR_SEND_RESP_IMM                             : constant := 1 * 2 ** 25;
   DCPR_CLEAR_MEMSTAT_DIS                         : constant := 1 * 2 ** 24;

   TGL_PCODE_MEM_SUBSYSTEM_INFO                   : constant := 16#d#;
   TGL_PCODE_MEM_SS_READ_GLOBAL_INFO              : constant := 0 * 2 ** 8;
   TGL_PCODE_CDCLK_CONTROL                        : constant := 7;
   TGL_CDCLK_PREPARE_FOR_CHANGE                   : constant := 3;
   TGL_CDCLK_READY_FOR_CHANGE                     : constant := 1;

   CDCLK_PLL_ENABLE_PLL_RATIO_MASK                : constant := 16#ff#;
   CDCLK_PLL_ENABLE_PLL_ENABLE                    : constant := 1 * 2 ** 31;
   CDCLK_PLL_ENABLE_PLL_LOCK                      : constant := 1 * 2 ** 30;
   CDCLK_CD2X_DIV_SEL_MASK                        : constant := 3 * 2 ** 22;
   CDCLK_CD2X_DIV_SEL_1                           : constant := 0 * 2 ** 22;
   CDCLK_CD2X_DIV_SEL_1_5                         : constant := 1 * 2 ** 22;
   CDCLK_CD2X_DIV_SEL_2                           : constant := 2 * 2 ** 22;
   CDCLK_CD2X_DIV_SEL_4                           : constant := 3 * 2 ** 22;
   CDCLK_CD2X_PIPE_NONE                           : constant := 7 * 2 ** 19;
   CDCLK_CTL_CD_FREQ_DECIMAL_MASK                 : constant := 16#7ff#;
   CDCLK_PLL_ENABLE_FREQ_REQ_ACK                  : constant := 1 * 2 ** 22;
   CDCLK_PLL_ENABLE_FREQ_REQ                      : constant := 1 * 2 ** 23;

   ----------------------------------------------------------------------------

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

   procedure Get_Max_CDClk (CDClk : out Config.CDClk_Range)
   is
      Refclk_Freq : Frequency_Type;
   begin
      Get_Refclk (Refclk_Freq);
      CDClk :=
        (case Refclk_Freq is
         when 24_000_000 => 648_000_000,
         when others     => 652_800_000);
   end Get_Max_CDClk;

   procedure Normalize_CDClk (CDClk : in Int64; Normalized: out Frequency_Type)
   with
      Post => Normalized >= 172_800_000
   is
      Refclk_Freq : Frequency_Type;
   begin
      Get_Refclk (Refclk_Freq);
      if Config.Has_CDClk_PLL_Crawl then
         Normalized :=
           (case Refclk_Freq is
            when 19_200_000 =>
              (if    CDClk <= 172_800_000 then 172_800_000
               elsif CDClk <= 192_000_000 then 192_000_000
               elsif CDClk <= 307_200_000 then 307_200_000
               elsif CDClk <= 556_800_000 then 556_800_000
                                          else 652_800_000),
            when 24_000_000 =>
              (if    CDClk <= 176_000_000 then 176_000_000
               elsif CDClk <= 192_000_000 then 192_000_000
               elsif CDClk <= 312_000_000 then 307_200_000
               elsif CDClk <= 552_000_000 then 552_000_000
                                          else 648_000_000),
            when others => -- 38_400_000
              (if    CDClk <= 179_200_000 then 179_200_000
               elsif CDClk <= 192_000_000 then 192_000_000
               elsif CDClk <= 307_200_000 then 307_200_000
               elsif CDClk <= 556_800_000 then 556_800_000
                                          else 652_800_000));
      else
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
      end if;
   end Normalize_CDClk;

   procedure Get_Cur_CDClk (CDClk : out Config.CDClk_Range)
   with
      Post => CDClk >= 172_800_000
   is
      CDCLK_CTL : Word32;
   begin
      Registers.Read (Registers.CDCLK_CTL, CDCLK_CTL);
      CDCLK_CTL := CDCLK_CTL and CDCLK_CTL_CD_FREQ_DECIMAL_MASK;
      Normalize_CDClk (Int64 (CDCLK_CTL) * 500_000 + 1_000_000, CDClk);
   end Get_Cur_CDClk;

   procedure Set_CDClk (CDClk_In : Frequency_Type)
   is
      subtype PLL_Ratio_Range is natural range 0 .. 68;
      function Ratio_For_19_2_MHz (CDClk : Frequency_Type) return PLL_Ratio_Range is
      begin
         if Config.Has_CDClk_PLL_Crawl then
            case CDClk is
               when 172_800_000 => return 27;
               when 192_000_000 => return 20;
               when 307_200_000 => return 32;
               when 556_800_000 => return 58;
               when 652_800_000 => return 68;
               when others      => return 0;
            end case;
         else
            case CDClk is
               when 172_800_000 => return 18;
               when 192_000_000 => return 20;
               when 307_200_000 => return 32;
               when 326_400_000
                  | 652_800_000 => return 68;
               when 556_800_000 => return 58;
               when others => return 0;
            end case;
         end if;
      end Ratio_For_19_2_MHz;

      function Ratio_For_38_4_MHz (CDClk : Frequency_Type) return PLL_Ratio_Range is
      begin
         if Config.Has_CDClk_PLL_Crawl then
            case CDClk is
               when 179_200_000 => return 14;
               when 192_000_000 => return 10;
               when 307_200_000 => return 16;
               when 556_800_000 => return 29;
               when 652_800_000 => return 34;
               when others      => return 0;
            end case;
         else
            case CDClk is
               when 172_800_000 => return 9;
               when 192_000_000 => return 10;
               when 307_200_000 => return 16;
               when 326_400_000
                  | 652_800_000 => return 34;
               when 556_800_000 => return 29;
               when others =>      return 0;
            end case;
         end if;
      end Ratio_For_38_4_MHz;

      function Ratio_For_24_MHz (CDClk : Frequency_Type) return PLL_Ratio_Range is
      begin
         if Config.Has_CDClk_PLL_Crawl then
            case CDClk is
               when 176_000_000 => return 22;
               when 192_000_000 => return 16;
               when 312_000_000 => return 26;
               when 552_000_000 => return 46;
               when 648_000_000 => return 54;
               when others      => return 0;
            end case;
         else
            case CDClk is
               when 180_800_000 => return 15;
               when 192_000_000 => return 16;
               when 312_000_000 => return 26;
               when 324_000_000
                  | 648_000_000 => return 54;
               when 552_000_000 => return 46;
               when others => return 0;
            end case;
         end if;
      end Ratio_For_24_MHz;

      function CDCLK_CTL_CD_FREQ_DECIMAL (Freq : Frequency_Type) return Word32 with
         Pre => Freq > 1_000_000
      is
      begin
         -- Weirdest representation: CDClk - 1MHz in 10.1 (10 + 1 fractional bit)
         return Word32 (Div_Round_Closest (Int64 (Freq) - 1_000_000, 500_000));
      end CDCLK_CTL_CD_FREQ_DECIMAL;

      Success : Boolean;
      CD2X : Word32;
      PLL_Ratio : PLL_Ratio_Range;
      CDClk : Config.CDClk_Range;
      Refclk_Freq : Refclk_Range;
      VCO : Pos64;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Normalize_CDClk (Frequency_Type'Min (CDClk_In, Config.Max_CDClk), CDClk);
      Get_Refclk (Refclk_Freq);
      case Refclk_Freq is
         when 19_200_000 => PLL_Ratio := Ratio_For_19_2_MHz (CDClk);
         when 38_400_000 => PLL_Ratio := Ratio_For_38_4_MHz (CDClk);
         when 24_000_000 => PLL_Ratio := Ratio_For_24_MHz (CDClk);
         when others => PLL_Ratio := 0;
      end case;

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

      if not Success and not Config.CPU_Alderlake then
         pragma Debug (Debug.Put_Line
                       ("ERROR: PCODE not ready for frequency change."));
         return;
      end if;

      if Config.Has_CDClk_PLL_Crawl then
         Registers.Write
           (Register => Registers.CDCLK_PLL_ENABLE,
            Value    => Word32(PLL_Ratio) or
                        CDCLK_PLL_ENABLE_PLL_ENABLE);
         Registers.Write
           (Register => Registers.CDCLK_PLL_ENABLE,
            Value    => Word32(PLL_Ratio) or
                        CDCLK_PLL_ENABLE_PLL_ENABLE or
                        CDCLK_PLL_ENABLE_FREQ_REQ);

         Registers.Wait_Set_Mask
           (Register  => Registers.CDCLK_PLL_ENABLE,
            Mask      => CDCLK_PLL_ENABLE_PLL_LOCK or
                         CDCLK_PLL_ENABLE_FREQ_REQ_ACK,
            Success  => Success);
         Registers.Write
           (Register => Registers.CDCLK_PLL_ENABLE,
            Value    => Word32(PLL_Ratio) or
                        CDCLK_PLL_ENABLE_PLL_ENABLE);
      else
         Registers.Unset_Mask
           (Register => Registers.CDCLK_PLL_ENABLE,
            Mask     => CDCLK_PLL_ENABLE_PLL_ENABLE);
         Registers.Wait_Unset_Mask
           (Register => Registers.CDCLK_PLL_ENABLE,
            Mask     => CDCLK_PLL_ENABLE_PLL_LOCK);

         Registers.Write
           (Register => Registers.CDCLK_PLL_ENABLE,
            Value    => Word32(PLL_Ratio));
         Registers.Write
           (Register => Registers.CDCLK_PLL_ENABLE,
            Value    => Word32(PLL_Ratio) or CDCLK_PLL_ENABLE_PLL_ENABLE);
         Registers.Wait_Set_Mask
           (Register => Registers.CDCLK_PLL_ENABLE,
            Mask     => CDCLK_PLL_ENABLE_PLL_LOCK,
            Success  => Success);
      end if;

      if not Success then
         Debug.Put_Line ("CDClk PLL failed to lock!");
         return;
      end if;

      VCO := (Refclk_Freq / 1_000) * Pos64(PLL_Ratio);
      CD2X :=
         (case (Div_Round_Closest (VCO, CDClk / 1_000)) is
          when 2 => CDCLK_CD2X_DIV_SEL_1,
          when 3 => CDCLK_CD2X_DIV_SEL_1_5,
          when 4 => CDCLK_CD2X_DIV_SEL_2,
          when 8 => CDCLK_CD2X_DIV_SEL_4,
          when others => CDCLK_CD2X_DIV_SEL_1);

      Registers.Write
        (Register => Registers.CDCLK_CTL,
         Value => (case CDClk is
                   when 168_000_000 => 16#14e#,
                   when 172_800_000 => 16#158#,
                   when 179_200_000 => 16#164#,
                   when 180_000_000 => 16#166#,
                   when 192_000_000 => 16#17e#,
                   when 307_200_000 => 16#264#,
                   when 312_000_000 => 16#26e#,
                   when 324_000_000 => 16#286#,
                   when 326_400_000 => 16#28b#,
                   when 480_000_000 => 16#3be#,
                   when 552_000_000 => 16#44e#,
                   when 556_800_000 => 16#458#,
                   when 648_000_000 => 16#50e#,
                   when 652_800_000 => 16#518#,
                   when others      => CDCLK_CTL_CD_FREQ_DECIMAL (CDClk)) or
                   CDCLK_CD2X_PIPE_NONE or
                   CD2X);

      PCode.Mailbox_Write
        (MBox     => TGL_PCODE_CDCLK_CONTROL,
         Command  => (if    CDClk <= 312_000_000 then 0
                      elsif CDClk <= 326_400_000 then 1
                      elsif CDClk <= 556_800_000 then 2
                      else 3),
         Wait_Ready => True);
      Config.CDClk := CDClk;
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
      type Bw_Buddy_Info_Array is array (Natural range 0 .. 3) of Bw_Buddy_Info;
      Buddy_Info : constant Bw_Buddy_Info_Array :=
        ((1, DDR4,   16#f#),
         (2, LPDDR4, 16#1c#),
         (2, DDR4,   16#1f#),
         (4, LPDDR4, 16#3c#));
      Result : Word64;
      Module_Type: DRAM_Module_Type;
      Channels : Natural;
      Success : Boolean;
      Found : Boolean := False;
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

            Registers.Unset_And_Set_Mask
              (Register   => Registers.BW_BUDDY1_CTL,
               Mask_Unset => BW_BUDDY_TLB_REQ_TIMER_MASK,
               Mask_Set   => 8 * 2 ** 16);
            Registers.Unset_And_Set_Mask
              (Register   => Registers.BW_BUDDY2_CTL,
               Mask_Unset => BW_BUDDY_TLB_REQ_TIMER_MASK,
               Mask_Set   => 8 * 2 ** 16);

            Found := True;
         end if;
         exit when Found;
      end loop;

      if not Found then
         Registers.Write (Registers.BW_BUDDY1_CTL, BW_BUDDY_DISABLE);
         Registers.Write (Registers.BW_BUDDY2_CTL, BW_BUDDY_DISABLE);
      end if;
   end Configure_Bandwidth_Buddy;

   ----------------------------------------------------------------------------

   procedure Initialize is
      procedure Get_Default_CDClk (CDClk: out Frequency_Type)
      is
         Refclk_Freq : Frequency_Type;
      begin
          Get_Refclk (Refclk_Freq);
          case Refclk_Freq is
             when 19_200_000 | 38_400_000 => CDClk := 172_800_000;
             when others                  => CDClk := 180_000_000;
          end case;
      end Get_Default_CDClk;
      Default_CDClk_Freq : Frequency_Type;
      CDClk : Frequency_Type;
      RawClk: Frequency_Type;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Config.Has_CDClk_PLL_Crawl then
         XELPD.Aux_Off;
      else
         TGL.Aux_Off;
      end if;

      -- Wa_14011294188:ehl,jsl,tgl,rkl,adl-s,adl-p
      Registers.Set_Mask
        (Register => Registers.PCH_DSPCLK_GATE_D,
         Mask     => PCH_DPMGUNIT_CLOCK_GATE_DISABLE);

      if Config.Has_CDClk_PLL_Crawl then
         -- Wa_22011091694:adlp
         Registers.Set_Mask
           (Register => Registers.GEN9_CLKGATE_DIS_5,
            Mask     => 1 * 2 ** 17); --DPCE_GATING_DIS);

         -- i915 calls this the "Bspec/49189 Initialize Sequence"
         Registers.Unset_Mask
           (Register => Registers.GEN8_CHICKEN_DCPR_1,
            Mask     => 1 * 2 ** 7); --DDI_CLOCK_REG_ACCESS);
      else
         -- Display WA #1185 WaDisableDARBFClkGating
         Registers.Set_Mask (Registers.GEN9_CLKGATE_DIS_0, DARBF_GATING_DIS);
      end if;

      Registers.Set_Mask
        (Register => Registers.NDE_RSTWRN_OPT,
         Mask     => NDE_RSTWRN_OPT_RST_PCH_Handshake_En);

      Combo_Phy.Initialize;

      if Config.Has_CDClk_PLL_Crawl then
         XELPD.Init_Power;
      else
         TGL.Init_Power;
      end if;

      Get_Cur_CDClk (CDClk);
      Config.CDClk := CDClk;
      Get_Max_CDClk (Config.Max_CDClk);
      Get_Default_CDClk (Default_CDClk_Freq);
      if Config.CDClk /= Default_CDClk_Freq then
         Set_CDClk (Default_CDClk_Freq);
      end if;

      Get_RawClk (Rawclk);
      Config.Raw_Clock := RawClk;

      if Config.Has_CDClk_PLL_Crawl then
         Registers.Unset_And_Set_Mask
           (Register   => Registers.MBUS_CTL,
            Mask_Unset => MBUS_HASHING_MODE_MASK or
                          MBUS_JOIN or
                          MBUS_JOIN_PIPE_SELECT_MASK,
            Mask_Set   => MBUS_HASHING_MODE_1x4 or
                          MBUS_JOIN or
                          MBUS_JOIN_PIPE_SELECT_NONE);

         Registers.Unset_And_Set_Mask
           (Register   => Registers.DBUF_CTL_S0,
            Mask_Unset => DBUF_CTL_MIN_TRACKER_STATE_SERVICE_MASK,
            Mask_Set   =>
               Shift_Left (3, DBUF_CTL_MIN_TRACKER_STATE_SERVICE_SHIFT));
      else
         Registers.Unset_And_Set_Mask
           (Register   => Registers.DBUF_CTL_S0,
            Mask_Unset => DBUF_CTL_TRACKER_STATE_SERVICE_MASK,
            Mask_Set   => Shift_Left (8, DBUF_CTL_TRACKER_STATE_SERVICE_SHIFT));
      end if;

      -- Enable required DBUF slices
      Registers.Set_Mask (Registers.DBUF_CTL_S0, DBUF_CTL_DBUF_POWER_REQUEST);
      Registers.Posting_Read (Registers.DBUF_CTL_S0);
      Registers.Wait_Set_Mask (Registers.DBUF_CTL_S0, DBUF_CTL_DBUF_POWER_STATE);

      if Config.Has_CDClk_PLL_Crawl then
         for I in 1 .. 3 loop
            Registers.Set_Mask (DBUF_Regs (I), DBUF_CTL_DBUF_POWER_REQUEST);
            Registers.Posting_Read (DBUF_Regs (I));
            Registers.Wait_Set_Mask (DBUF_Regs (I), DBUF_CTL_DBUF_POWER_STATE);
         end loop;
      end if;

      if not Config.Has_CDClk_PLL_Crawl then
         for I in MBUS_ABOX_CTL'Range loop
            Registers.Unset_And_Set_Mask
              (Register   => MBUS_ABOX_CTL (I),
               Mask_Unset => MBUS_ABOX_MASK,
               Mask_Set   => MBUS_ABOX_CREDITS);
         end loop;
      end if;

      Configure_Bandwidth_Buddy;

      -- Display WA #14011508470 tgl,dg1,rkl,adl-s,adl-p
      Registers.Set_Mask
        (Register => Registers.GEN11_CHICKEN_DCPR_2,
         Mask     => DCPR_MASK_MAXLATENCY_MEMUP_CLR or
                     DCPR_MASK_LPMODE or
                     DCPR_SEND_RESP_IMM or
                     DCPR_CLEAR_MEMSTAT_DIS);

      if Config.Has_CDClk_PLL_Crawl then
         Registers.Write
           (Register => Registers.DISPLAY_ERR_FATAL_MASK,
            Value    => 16#ffff_ffff#);
      end if;
   end Initialize;

   procedure Limit_Dotclocks
     (Configs        : in out Pipe_Configs;
      CDClk_Switch   :    out Boolean)
   is
      CDClk : Frequency_Type;
   begin
      Config_Helpers.Limit_Dotclocks (Configs, Config.Max_CDClk);
      Normalize_CDClk (Config_Helpers.Highest_Dotclock (Configs), CDClk);
      CDClk_Switch := Config.CDClk /= CDClk;
   end Limit_Dotclocks;

   procedure Update_CDClk (Configs : in out Pipe_Configs)
   is
      New_CDClk : Frequency_Type;
   begin
      Normalize_CDClk
         (Div_Round_Up (Config_Helpers.Highest_Dotclock (Configs), 2),
         New_CDClk);
      Set_CDClk (New_CDClk);
      Config_Helpers.Limit_Dotclocks (Configs, Config.CDClk);
   end Update_CDClk;

   procedure Enable_CDClk
   is
      Refclk_Freq : Frequency_Type;
      Bypass_Freq : Frequency_Type;
   begin
      Get_Refclk (Refclk_Freq);

      if Config.CDClk = Refclk_Freq then
         Bypass_Freq := Refclk_Freq / 2;
         Set_CDClk (Bypass_Freq);
      end if;
   end Enable_CDClk;

   procedure Power_Set_To (Configs : Pipe_Configs) is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Config.Has_XELPD_Power_Domains then
         XELPD.Power_Set_To (Configs);
      else
         TGL.Power_Set_To (Configs);
      end if;
   end Power_Set_To;

   procedure Power_Up (Old_Configs, New_Configs : Pipe_Configs) is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Config.Has_XELPD_Power_Domains then
         XELPD.Power_Up (Old_Configs, New_Configs);
      else
         TGL.Power_Up (Old_Configs, New_Configs);
      end if;
   end Power_Up;

   procedure Power_Down (Old_Configs, Tmp_Configs, New_Configs : Pipe_Configs)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Config.Has_XELPD_Power_Domains then
         XELPD.Power_Down (Old_Configs, Tmp_Configs, New_Configs);
      else
         TGL.Power_Down (Old_Configs, Tmp_Configs, New_Configs);
      end if;
   end Power_Down;

   procedure Pre_All_Off is
   begin
      Transcoder.PSR_Off;
   end Pre_All_Off;

   procedure Post_All_Off is
      Refclk : Frequency_Type;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for I in reverse DBUF_Regs'Range loop
         Registers.Unset_Mask
            (DBUF_Regs (I), DBUF_CTL_DBUF_POWER_REQUEST);
         Registers.Wait_Unset_Mask
            (DBUF_Regs (I), DBUF_CTL_DBUF_POWER_STATE);
      end loop;

      -- Disable CD clock, bypass frequency is Refclk / 2
      Get_Refclk (Refclk);
      Set_CDClk (Refclk / 2);

      if Config.Has_XELPD_Power_Domains then
         XELPD.All_Off;
      else
         TGL.All_Off;
      end if;

      Combo_Phy.All_Off;
   end Post_All_Off;

   procedure Power_Up_Aux is
   begin
      if Config.Has_XELPD_Power_Domains then
         XELPD.Power_Up_Aux;
      end if;
   end Power_Up_Aux;

end HW.GFX.GMA.Power_And_Clocks;
