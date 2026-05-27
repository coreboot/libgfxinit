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

use type HW.Word64;

package body HW.GFX.GMA.Power_And_Clocks is

   subtype CDClk_Range is Config.CDClk_Range;

   ----------------------------------------------------------------------------

   PCH_DPMGUNIT_CLOCK_GATE_DISABLE                : constant := 1 * 2 ** 15;
   NDE_RSTWRN_OPT_RST_PCH_Handshake_En            : constant := 1 * 2 ** 4;

   ----------------------------------------------------------------------------

   DBUF_CTL_DBUF_POWER_REQUEST                    : constant := 1 * 2 ** 31;
   DBUF_CTL_TRACKER_STATE_SERVICE_MASK            : constant := 16#f8_0000#;
   DBUF_CTL_TRACKER_STATE_SERVICE_SHIFT           : constant := 19;
   DBUF_CTL_MIN_TRACKER_STATE_SERVICE_SHIFT       : constant := 16;
   DBUF_CTL_MIN_TRACKER_STATE_SERVICE_MASK        : constant := 16#7_0000#;
   DBUF_CTL_DBUF_POWER_STATE                      : constant := 1 * 2 ** 30;

   type DBUF_Slices is (S1, S2, S3, S4);
   subtype Enable_DBUF_Slices is DBUF_Slices -- TODO: Maybe decide at runtime?
      range S1 .. (if Config.Gen_Tigerlake then S1 else S4);
   subtype Disable_DBUF_Slices is DBUF_Slices
      range S1 .. (if Config.Gen_Tigerlake then S2 else S4);
   DBUF_CTL : constant array (DBUF_Slices) of Registers.Registers_Index :=
     (Registers.DBUF_CTL_S0,
      Registers.DBUF_CTL_S1,
      Registers.DBUF_CTL_S2,
      Registers.DBUF_CTL_S3);

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

   MBUS_JOIN                                      : constant := 1 * 2 ** 31;
   MBUS_HASHING_MODE_MASK                         : constant := 1 * 2 ** 30;
   MBUS_HASHING_MODE_2x2                          : constant := 0 * 2 ** 30;
   MBUS_HASHING_MODE_1x4                          : constant := 1 * 2 ** 30;
   MBUS_JOIN_PIPE_SELECT_MASK                     : constant := 16#1c00_0000#;
   MBUS_JOIN_PIPE_SELECT_NONE                     : constant := 16#1c00_0000#;

   ----------------------------------------------------------------------------

   DCPR_MASK_MAXLATENCY_MEMUP_CLR                 : constant := 1 * 2 ** 27;
   DCPR_MASK_LPMODE                               : constant := 1 * 2 ** 26;
   DCPR_SEND_RESP_IMM                             : constant := 1 * 2 ** 25;
   DCPR_CLEAR_MEMSTAT_DIS                         : constant := 1 * 2 ** 24;

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
   CDCLK_CD2X_DIV_SEL_1_5                         : constant := 1 * 2 ** 22;
   CDCLK_CD2X_DIV_SEL_2                           : constant := 2 * 2 ** 22;
   CDCLK_CD2X_DIV_SEL_4                           : constant := 3 * 2 ** 22;
   CDCLK_CD2X_PIPE_NONE                           : constant := 7 * 2 ** 19;
   CDCLK_CTL_CD_FREQ_DECIMAL_MASK                 : constant := 16#7ff#;

   ----------------------------------------------------------------------------

   procedure Get_RefClk (RefClk : out RefClk_Range)
   is
      DSSM : Word32;
      DSSM_REFERENCE_FREQUENCY_MASK    : constant := 16#e000_0000#;
      DSSM_REFERENCE_FREQUENCY_24MHZ   : constant := 16#0000_0000#;
      DSSM_REFERENCE_FREQUENCY_19_2MHZ : constant := 16#2000_0000#;
      DSSM_REFERENCE_FREQUENCY_38_4MHZ : constant := 16#4000_0000#;
   begin
      Registers.Read (Registers.DSSM, DSSM);
      RefClk :=
        (case DSSM and DSSM_REFERENCE_FREQUENCY_MASK is
         when DSSM_REFERENCE_FREQUENCY_24MHZ   => 24_000_000,
         when DSSM_REFERENCE_FREQUENCY_19_2MHZ => 19_200_000,
         when DSSM_REFERENCE_FREQUENCY_38_4MHZ => 38_400_000,
         when others                           => 24_000_000);
   end Get_RefClk;

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
      RefClk_Freq : RefClk_Range;
   begin
      Get_RefClk (RefClk_Freq);
      CDClk :=
        (case RefClk_Freq is
         when 24_000_000 => 648_000_000,
         when others     => 652_800_000);
   end Get_Max_CDClk;

   procedure Normalize_CDClk
     (CDClk       : in     Int64;
      Normalized  :    out CDClk_Range)
   with
      Post => Normalized >= 172_800_000
   is
      RefClk_Freq : RefClk_Range;
   begin
      Get_RefClk (RefClk_Freq);
      Normalized :=
        (if Config.Gen_Tigerlake then
           (case RefClk_Freq is
               when 19_200_000 | 38_400_000 =>
                  (if    CDClk <= 172_800_000 then 172_800_000
                   elsif CDClk <= 192_000_000 then 192_000_000
                   elsif CDClk <= 307_200_000 then 307_200_000
                   elsif CDClk <= 326_400_000 then 326_400_000
                   elsif CDClk <= 556_800_000 then 556_800_000
                                              else 652_800_000),
               when others => -- 24_000_000
                  (if    CDClk <= 180_000_000 then 180_000_000
                   elsif CDClk <= 192_000_000 then 192_000_000
                   elsif CDClk <= 312_000_000 then 312_000_000
                   elsif CDClk <= 324_000_000 then 324_000_000
                   elsif CDClk <= 552_000_000 then 552_000_000
                                              else 648_000_000))
         else
           (case RefClk_Freq is
               when 19_200_000 =>
                 (if    CDClk <= 172_800_000 then 172_800_000
                  elsif CDClk <= 192_000_000 then 192_000_000
                  elsif CDClk <= 307_200_000 then 307_200_000
                  elsif CDClk <= 556_800_000 then 556_800_000
                                             else 652_800_000),
               when 24_000_000 =>
                 (if    CDClk <= 176_000_000 then 176_000_000
                  elsif CDClk <= 192_000_000 then 192_000_000
                  elsif CDClk <= 312_000_000 then 312_000_000
                  elsif CDClk <= 552_000_000 then 552_000_000
                                             else 648_000_000),
               when others => -- 38_400_000
                 (if    CDClk <= 179_200_000 then 179_200_000
                  elsif CDClk <= 192_000_000 then 192_000_000
                  elsif CDClk <= 307_200_000 then 307_200_000
                  elsif CDClk <= 556_800_000 then 556_800_000
                                             else 652_800_000)));
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
      function Ratio_For_19_2_MHz (CDClk : CDClk_Range) return PLL_Ratio_Range
      is
        (case CDClk is
            when 172_800_000 =>
              (if Config.Gen_Tigerlake
                           then 18
                           else 27),-- ADL_P: only used with 19.2MHz
            when 179_200_000 => 28, -- ADL_P: only used with 38.4MHz
            when 192_000_000 => 20,
            when 307_200_000 => 32,
            when 326_400_000 | 652_800_000 => 68,
            when 556_800_000 => 58,
            when others => 0);

      function Ratio_For_24_MHz (CDClk : CDClk_Range) return PLL_Ratio_Range
      is
        (case CDClk is
            when 176_000_000 => 22, -- only ADL_P
            when 180_000_000 => 15, -- only TGL
            when 192_000_000 => 16,
            when 312_000_000 => 26,
            when 324_000_000 | 648_000_000 => 54,
            when 552_000_000 => 46,
            when others => 0);

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
      RefClk_Freq : RefClk_Range;
      VCO : Pos64;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Normalize_CDClk (CDClk_Range'Min (CDClk_In, Config.Max_CDClk), CDClk);
      Get_RefClk (RefClk_Freq);
      PLL_Ratio := (case RefClk_Freq is
         when 19_200_000 => Ratio_For_19_2_MHz (CDClk),
         when 38_400_000 => Ratio_For_19_2_MHz (CDClk) / 2,
         when 24_000_000 => Ratio_For_24_MHz (CDClk),
         when others     => 0);

      if PLL_Ratio = 0 then
         pragma Debug (Debug.Put_Line
                       ("ERROR: Invalid RefClk frequency, bad hardware?"));
         return;
      end if;

      PCode.Mailbox_Request
        (MBox       => TGL_PCODE_CDCLK_CONTROL,
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

      VCO := (RefClk_Freq / 1_000) * Pos64 (PLL_Ratio);
      CD2X :=
         (case (Div_Round_Closest (VCO, CDClk / 1_000)) is
          when 2 => CDCLK_CD2X_DIV_SEL_1,
          when 3 => CDCLK_CD2X_DIV_SEL_1_5,
          when 4 => CDCLK_CD2X_DIV_SEL_2,
          when 8 => CDCLK_CD2X_DIV_SEL_4,
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

      case Result and 16#f# is
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

            if Config.Gen_Tigerlake then
               -- Wa_22010178259:tgl,rkl
               Registers.Unset_And_Set_Mask
                 (Register   => Registers.BW_BUDDY1_CTL,
                  Mask_Unset => BW_BUDDY_TLB_REQ_TIMER_MASK,
                  Mask_Set   => 8 * 2 ** 16);
               Registers.Unset_And_Set_Mask
                 (Register   => Registers.BW_BUDDY2_CTL,
                  Mask_Unset => BW_BUDDY_TLB_REQ_TIMER_MASK,
                  Mask_Set   => 8 * 2 ** 16);
            end if;

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

      Power_Domains_Common.PD_On (Power_Domains.PW1);

      Get_Cur_CDClk (Config.CDClk);
      Get_Max_CDClk (Config.Max_CDClk);
      if Config.CDClk < Config.Default_CDClk_Freq then
         Set_CDClk (Config.Default_CDClk_Freq);
      end if;

      Get_RawClk (RawClk);
      Config.Raw_Clock := RawClk;

      -- Settings for joined MBUS. MBUS joining is usually used when buffers
      -- from both DBUF slice pairs 1/2 and 3/4 are used for a single pipe.
      -- TODO: Investigate if this is needed for our PLANE_BUF_CFG allocation.
      if Config.Has_Mbus_Joining then
         Registers.Unset_And_Set_Mask
           (Register   => Registers.MBUS_CTL,
            Mask_Unset => MBUS_HASHING_MODE_MASK or
                          MBUS_JOIN or
                          MBUS_JOIN_PIPE_SELECT_MASK,
            Mask_Set   => MBUS_HASHING_MODE_1x4 or
                          MBUS_JOIN or
                          MBUS_JOIN_PIPE_SELECT_NONE);
      end if;

      -- TGL: Set DBUF Tracker State Service to 8
      if Config.Gen_Tigerlake then
         Registers.Unset_And_Set_Mask
           (Register    => DBUF_CTL (S1),
            Mask_Unset  => DBUF_CTL_TRACKER_STATE_SERVICE_MASK,
            Mask_Set    => 8 * 2 ** DBUF_CTL_TRACKER_STATE_SERVICE_SHIFT);
      end if;

      if Config.Gen_AlderlakeP then
         Registers.Unset_And_Set_Mask
           (Register    => DBUF_CTL (S1),
            Mask_Unset  => DBUF_CTL_MIN_TRACKER_STATE_SERVICE_MASK,
            Mask_Set    => 3 * 2 ** DBUF_CTL_MIN_TRACKER_STATE_SERVICE_SHIFT);
      end if;

      -- Enable required DBUF slices
      for S in Enable_DBUF_Slices loop
         Registers.Set_Mask (DBUF_CTL (S), DBUF_CTL_DBUF_POWER_REQUEST);
         Registers.Wait_Set_Mask (DBUF_CTL (S), DBUF_CTL_DBUF_POWER_STATE);
      end loop;

      if Config.Has_Mbus_Abox_Credits then
         for I in MBUS_ABOX_CTL'Range loop
            Registers.Unset_And_Set_Mask
              (Register    => MBUS_ABOX_CTL (I),
               Mask_Unset  => MBUS_ABOX_MASK,
               Mask_Set    => MBUS_ABOX_CREDITS);
         end loop;
      end if;

      Configure_Bandwidth_Buddy;

      -- Display WA #14011508470 tgl,dg1,rkl,adl-s,adl-p,dg2
      Registers.Set_Mask
        (Register => Registers.GEN11_CHICKEN_DCPR_2,
         Mask     => DCPR_MASK_MAXLATENCY_MEMUP_CLR or DCPR_MASK_LPMODE or
                     DCPR_SEND_RESP_IMM or DCPR_CLEAR_MEMSTAT_DIS);

      if Config.Gen_AlderlakeP then
         declare
            DPCE_GATING_DIS      : constant := 1 * 2 ** 17;
            DDI_CLOCK_REG_ACCESS : constant := 1 * 2 **  7;
         begin
            -- Display WA #14011503030 xelpd
            Registers.Write
              (Register => Registers.DISPLAY_ERR_FATAL_MASK,
               Value    => 16#ffff_ffff#);

            -- Wa_22011091694:adlp
            Registers.Set_Mask
              (Register => Registers.GEN9_CLKGATE_DIS_5,
               Mask     => DPCE_GATING_DIS);

            -- Bspec/49189 Initialize Sequence
            Registers.Unset_Mask
              (Register => Registers.GEN8_CHICKEN_DCPR_1,
               Mask     => DDI_CLOCK_REG_ACCESS);
         end;
      end if;
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

   procedure Pre_All_Off is
   begin
      Transcoder.PSR_Off;
   end Pre_All_Off;

   procedure Post_All_Off is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for S in reverse Disable_DBUF_Slices loop
         Registers.Unset_Mask (DBUF_CTL (S), DBUF_CTL_DBUF_POWER_REQUEST);
         Registers.Wait_Unset_Mask (DBUF_CTL (S), DBUF_CTL_DBUF_POWER_STATE);
      end loop;

      -- Disable CDClk PLL. FIXME: Not implemented yet.
      Set_CDClk (CDClk_Range'First);

      Power_Domains_Common.All_Off;

      Combo_Phy.All_Off;
   end Post_All_Off;

end HW.GFX.GMA.Power_And_Clocks;
