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

with HW.Time;
with HW.GFX.GMA.Config;
with HW.GFX.GMA.Registers;

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.PLLs
with
   Refined_State => (State => PLLs)
is

   Debug_Clocks : constant Boolean := False;

   type Count_Range is new Natural range 0 .. 2;

   type PLL_State is record
      Use_Count   : Count_Range;
      Mode        : Mode_Type;
   end record;

   type PLL_State_Array is array (DPLLs) of PLL_State;

   PLLs : PLL_State_Array;

   ----------------------------------------------------------------------------

   -- i945 PLL limits use "actual" divider values (register_value + 2).
   -- Linux kernel i9xx limits (register values):
   --   N: 1..6, M1: 8..18, M2: 3..7
   -- Actual values (register + 2):
   --   N: 3..8, M1: 10..20, M2: 5..9
   -- M = 5 * M1 + M2, range 70..120
   -- VCO = ref_clk * M / N, range 1400..2800 MHz
   -- Reference clock: 96 MHz

   subtype N_Range     is Int64 range          3 ..          8;
   subtype M_Range     is Int64 range         70 ..        120;
   subtype M1_Range    is Int64 range         10 ..         20;
   subtype M2_Range    is Int64 range          5 ..          9;
   subtype P_Range     is Int64 range          5 ..         98;
   subtype P1_Range    is Int64 range          1 ..          8;
   subtype P2_Range    is Int64 range          5 ..         14;
   subtype VCO_Range   is Int64 range 1400000000 .. 2800000000;
   subtype Clock_Range is HW.GFX.Frequency_Type;

   type Clock_Type is
      record
         N               : N_Range;
         M1              : M1_Range;
         M2              : M2_Range;
         P1              : P1_Range;
         P2              : P2_Range;
         M               : M_Range;
         P               : P_Range;
         VCO             : VCO_Range;
         Reference_Clock : Clock_Range;
         Dotclock        : Clock_Range;
      end record;

   Invalid_Clock : constant Clock_Type := Clock_Type'
      (N               => N_Range'Last,
       M1              => M1_Range'Last,
       M2              => M2_Range'Last,
       P1              => P1_Range'Last,
       P2              => P2_Range'Last,
       Reference_Clock => Clock_Range'Last,
       M               => M_Range'Last,
       P               => P_Range'Last,
       VCO             => VCO_Range'Last,
       Dotclock        => Clock_Range'Last);

   type Limits_Type is
      record
         N_Lower      : N_Range;
         N_Upper      : N_Range;
         M_Lower      : M_Range;
         M_Upper      : M_Range;
         M1_Lower     : M1_Range;
         M1_Upper     : M1_Range;
         M2_Lower     : M2_Range;
         M2_Upper     : M2_Range;
         P_Lower      : P_Range;
         P_Upper      : P_Range;
         P1_Lower     : P1_Range;
         P1_Upper     : P1_Range;
         P2_Fast      : P2_Range;
         P2_Slow      : P2_Range;
         P2_Threshold : Clock_Range;
         VCO_Lower    : VCO_Range;
         VCO_Upper    : VCO_Range;
      end record;

   -- i9xx LVDS limits (from Linux intel_limits_i9xx_lvds)
   LVDS_Limits : constant Limits_Type := Limits_Type'
     (N_Lower      =>   3,           N_Upper   =>   8,
      M_Lower      =>  70,           M_Upper   => 120,
      M1_Lower     =>  10,           M1_Upper  =>  20,
      M2_Lower     =>   5,           M2_Upper  =>   9,
      P_Lower      =>   7,           P_Upper   =>  98,
      P1_Lower     =>   1,           P1_Upper  =>   8,
      P2_Fast      =>   7,           P2_Slow   =>  14,
      P2_Threshold => 112_000_000,
      VCO_Lower    => 1_400_000_000, VCO_Upper => 2_800_000_000);

   -- i9xx SDVO/DAC limits (from Linux intel_limits_i9xx_sdvo)
   SDVO_DAC_Limits : constant Limits_Type := Limits_Type'
     (N_Lower      =>   3,           N_Upper   =>   8,
      M_Lower      =>  70,           M_Upper   => 120,
      M1_Lower     =>  10,           M1_Upper  =>  20,
      M2_Lower     =>   5,           M2_Upper  =>   9,
      P_Lower      =>   5,           P_Upper   =>  80,
      P1_Lower     =>   1,           P1_Upper  =>   8,
      -- use P2_Slow if Dotclock <= P2_Threshold, P2_Fast otherwise
      P2_Fast      =>   5,           P2_Slow   =>  10,
      P2_Threshold => 200_000_000,
      VCO_Lower    => 1_400_000_000, VCO_Upper => 2_800_000_000);

   -- Pineview has a different Gen3 DPLL encoding. Its N divider is encoded
   -- as a one-hot value in the frequency-parameter (FP) register, M1 is
   -- reserved as 0, M = M2 + 2, and P1 starts at bit 15.

   subtype PNV_N_Range     is Int64 range          3 ..          6;
   subtype PNV_M_Range     is Int64 range          2 ..        256;
   subtype PNV_M2_Range    is Int64 range          0 ..        254;
   subtype PNV_P_Range     is Int64 range          5 ..        112;
   subtype PNV_P1_Range    is Int64 range          1 ..          8;
   subtype PNV_P2_Range    is Int64 range          5 ..         14;
   subtype PNV_VCO_Range   is Int64 range 1700000000 .. 3500000000;

   type PNV_Clock_Type is
      record
         N               : PNV_N_Range;
         M2              : PNV_M2_Range;
         P1              : PNV_P1_Range;
         P2              : PNV_P2_Range;
         M               : PNV_M_Range;
         P               : PNV_P_Range;
         VCO             : PNV_VCO_Range;
         Reference_Clock : Clock_Range;
         Dotclock        : Clock_Range;
      end record;

   Invalid_PNV_Clock : constant PNV_Clock_Type := PNV_Clock_Type'
      (N               => PNV_N_Range'Last,
       M2              => PNV_M2_Range'Last,
       P1              => PNV_P1_Range'Last,
       P2              => PNV_P2_Range'Last,
       Reference_Clock => Clock_Range'Last,
       M               => PNV_M_Range'Last,
       P               => PNV_P_Range'Last,
       VCO             => PNV_VCO_Range'Last,
       Dotclock        => Clock_Range'Last);

   type PNV_Limits_Type is
      record
         M_Lower      : PNV_M_Range;
         M_Upper      : PNV_M_Range;
         M2_Lower     : PNV_M2_Range;
         M2_Upper     : PNV_M2_Range;
         P_Lower      : PNV_P_Range;
         P_Upper      : PNV_P_Range;
         P1_Lower     : PNV_P1_Range;
         P1_Upper     : PNV_P1_Range;
         P2_Fast      : PNV_P2_Range;
         P2_Slow      : PNV_P2_Range;
         P2_Threshold : Clock_Range;
      end record;

   -- Pineview SDVO/DAC limits (from Linux pnv_limits_sdvo)
   Pineview_SDVO_DAC_Limits : constant PNV_Limits_Type := PNV_Limits_Type'
     (M_Lower      =>   2,           M_Upper   => 256,
      M2_Lower     =>   0,           M2_Upper  => 254,
      P_Lower      =>   5,           P_Upper   =>  80,
      P1_Lower     =>   1,           P1_Upper  =>   8,
      P2_Fast      =>   5,           P2_Slow   =>  10,
      P2_Threshold => 200_000_000);

   -- Pineview LVDS limits (from Linux pnv_limits_lvds)
   Pineview_LVDS_Limits : constant PNV_Limits_Type := PNV_Limits_Type'
     (M_Lower      =>   2,           M_Upper   => 256,
      M2_Lower     =>   0,           M2_Upper  => 254,
      P_Lower      =>   7,           P_Upper   => 112,
      P1_Lower     =>   1,           P1_Upper  =>   8,
      P2_Fast      =>  14,           P2_Slow   =>  14,
      P2_Threshold => 112_000_000);

   ----------------------------------------------------------------------------

   type Regs is array (DPLLs) of Registers.Registers_Index;

   DPLL : constant Regs := Regs'(Registers.GMCH_DPLL_A, Registers.GMCH_DPLL_B);
   DPLL_VCO_ENABLE         : constant := 1 * 2 ** 31;
   DPLL_VGA_MODE_DIS       : constant := 1 * 2 ** 28;
   DPLL_P2_10_OR_14        : constant := 0 * 2 ** 24;
   DPLL_P2_5_OR_7          : constant := 1 * 2 ** 24;
   DPLL_P1_DIVIDER_SHIFT          : constant := 16;
   DPLL_P1_DIVIDER_SHIFT_PINEVIEW : constant := 15;
   DPLL_SDVOCLK            : constant := 2 * 2 ** 13;

   -- i945 does not use DPLL_PULSE_PHASE (bits 12:9, Gen4+ only)
   -- i945 uses DVO_2X_MODE (bit 30) for SDVO outputs (same bit as
   -- DPLL_SDVO_HIGH_SPEED on Gen4+)
   DPLL_DVO_2X_MODE : constant := 1 * 2 ** 30;
   DPLL_MODE_LVDS   : constant := 2 * 2 ** 26;
   DPLL_MODE_DAC    : constant := 1 * 2 ** 26;
   DPLL_DREFCLK     : constant := 0 * 2 ** 13;
   DPLL_SSC         : constant := 3 * 2 ** 13;

   MODE_DPLL_DAC : constant Word32 := Word32'
     (DPLL_MODE_DAC or DPLL_DREFCLK);

   MODE_DPLL_SDVO : constant Word32 := Word32'
     (DPLL_MODE_DAC or DPLL_DREFCLK or DPLL_DVO_2X_MODE);

   MODE_DPLL_LVDS : constant Word32 := Word32'
      (DPLL_MODE_LVDS or DPLL_SSC);

   type DPLL_Mode_Array is array (Display_Type) of Word32;

   DPLL_Mode : constant DPLL_Mode_Array := DPLL_Mode_Array'
     (LVDS     => MODE_DPLL_LVDS,
      VGA      => MODE_DPLL_DAC,
      HDMI     => MODE_DPLL_SDVO,  -- SDVO outputs use HDMI display type
      others   => MODE_DPLL_SDVO);

   FP0 : constant Regs := Regs'(Registers.GMCH_FPA0, Registers.GMCH_FPB0);
   FP1 : constant Regs := Regs'(Registers.GMCH_FPA1, Registers.GMCH_FPB1);
   FP_N_SHIFT            : constant := 16;
   FP_M1_SHIFT           : constant := 8;
   FP_M2_SHIFT           : constant := 0;

   ----------------------------------------------------------------------------

   procedure Verify_Parameters
      (N               : in     N_Range;
       M1              : in     M1_Range;
       M2              : in     M2_Range;
       P1              : in     P1_Range;
       P2              : in     P2_Range;
       Reference_Clock : in     Clock_Range;
       Current_Limits  : in     Limits_Type;
       Result          :    out Clock_Type;
       Valid           :    out Boolean)
   with
      Global => null,
      Pre => True,
      Post => True
   is
      M        : Int64;
      P        : Int64;
      VCO      : Int64;
      Dotclock : Int64;
   begin
      pragma Debug (Debug_Clocks, Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      M        := 5 * M1 + M2;
      P        := P1 * P2;
      VCO      := (Int64 (Reference_Clock) * M) / N;
      Dotclock := VCO / P;

      pragma Debug (Debug_Clocks and not (Current_Limits.P1_Lower  <= P1  and P1  <= Current_Limits.P1_Upper ), Debug.Put_Line ("P1 out of range."));
      pragma Debug (Debug_Clocks and     (Current_Limits.P2_Fast   /= P2  and P2  /= Current_Limits.P2_Slow  ), Debug.Put_Line ("P2 out of range."));
      pragma Debug (Debug_Clocks and not (Current_Limits.P_Lower   <= P   and P   <= Current_Limits.P_Upper  ), Debug.Put_Line ("P out of range."));
      pragma Debug (Debug_Clocks and not (Current_Limits.M1_Lower  <= M1  and M1  <= Current_Limits.M1_Upper ), Debug.Put_Line ("M1 out of range."));
      pragma Debug (Debug_Clocks and not (Current_Limits.M2_Lower  <= M2  and M2  <= Current_Limits.M2_Upper ), Debug.Put_Line ("M2 out of range."));
      pragma Debug (Debug_Clocks and not (Current_Limits.N_Lower   <= N   and N   <= Current_Limits.N_Upper  ), Debug.Put_Line ("N out of range."));
      pragma Debug (Debug_Clocks and not (Current_Limits.M_Lower   <= M   and M   <= Current_Limits.M_Upper  ), Debug.Put_Line ("M out of range."));
      pragma Debug (Debug_Clocks and not (Current_Limits.VCO_Lower <= VCO and VCO <= Current_Limits.VCO_Upper), Debug.Put_Line ("VCO out of range."));

      pragma Debug (Debug_Clocks and not (Int64 (Clock_Range'First) <= Dotclock),       Debug.Put_Line ("Dotclock too low."));
      pragma Debug (Debug_Clocks and not (Int64 (Clock_Range'First) <= Dotclock),       Debug.Put_Int64 (Dotclock));
      pragma Debug (Debug_Clocks and not (Int64 (Clock_Range'First) <= Dotclock),       Debug.New_Line);

      pragma Debug (Debug_Clocks and not (Dotclock <= Int64 (Clock_Range'Last)),        Debug.Put_Line ("Dotclock too high."));
      pragma Debug (Debug_Clocks and not (Dotclock <= Int64 (Clock_Range'Last)),        Debug.Put_Int64 (Dotclock));
      pragma Debug (Debug_Clocks and not (Dotclock <= Int64 (Clock_Range'Last)),        Debug.New_Line);

      Valid :=
         Current_Limits.P1_Lower  <= P1  and P1  <= Current_Limits.P1_Upper  and
         (Current_Limits.P2_Fast   = P2   or P2   = Current_Limits.P2_Slow)  and
         Current_Limits.P_Lower   <= P   and P   <= Current_Limits.P_Upper   and
         Current_Limits.M1_Lower  <= M1  and M1  <= Current_Limits.M1_Upper  and
         Current_Limits.M2_Lower  <= M2  and M2  <= Current_Limits.M2_Upper  and
         Current_Limits.N_Lower   <= N   and N   <= Current_Limits.N_Upper   and
         Current_Limits.M_Lower   <= M   and M   <= Current_Limits.M_Upper   and
         Current_Limits.VCO_Lower <= VCO and VCO <= Current_Limits.VCO_Upper and
         Int64 (Clock_Range'First) <= Dotclock                               and
         Dotclock <= Int64 (Clock_Range'Last);

      if Valid
      then
         Result := Clock_Type'
            (N               => N,
             M1              => M1,
             M2              => M2,
             P1              => P1,
             P2              => P2,
             Reference_Clock => Reference_Clock,
             M               => M,
             P               => P,
             VCO             => VCO,
             Dotclock        => Clock_Range (Dotclock));
      else
         Result := Invalid_Clock;
      end if;

   end Verify_Parameters;

   procedure Calculate_Clock_Parameters
     (Display         : in     Display_Type;
      Target_Dotclock : in     Clock_Range;
      Reference_Clock : in     Clock_Range;
      Best_Clock      :    out Clock_Type;
      Valid           :    out Boolean)
   with
     Global => null,
     Pre => True,
     Post => True
   is
      Limits : constant Limits_Type :=
      (case Display is
          when LVDS   => LVDS_Limits,
          when others => SDVO_DAC_Limits);

      P2               : P2_Range;
      Best_Delta       : Int64 := Int64'Last;
      Current_Delta    : Int64;
      Current_Clock    : Clock_Type;
      Registers_Valid  : Boolean;
   begin
      pragma Debug (Debug_Clocks, Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Valid      := False;
      Best_Clock := Invalid_Clock;

      if Target_Dotclock <= Limits.P2_Threshold then
         P2 := Limits.P2_Slow;
      else
         P2 := Limits.P2_Fast;
      end if;

      for N in N_Range range Limits.N_Lower .. Limits.N_Upper
      loop
         -- reverse loops as hardware prefers higher values
         for M1 in reverse M1_Range range Limits.M1_Lower .. Limits.M1_Upper
         loop
            pragma Loop_Invariant (True);
            for M2 in reverse M2_Range range Limits.M2_Lower .. Int64'Min (Limits.M2_Upper, M1)
            loop
               pragma Loop_Invariant (True);
               for P1 in reverse P1_Range range Limits.P1_Lower .. Limits.P1_Upper
               loop
                  Verify_Parameters
                    (N               => N,
                     M1              => M1,
                     M2              => M2,
                     P1              => P1,
                     P2              => P2,
                     Reference_Clock => Reference_Clock,
                     Current_Limits  => Limits,
                     Result          => Current_Clock,
                     Valid           => Registers_Valid);

                  if Registers_Valid
                  then
                     if Current_Clock.Dotclock > Target_Dotclock
                     then
                        Current_Delta := Current_Clock.Dotclock - Target_Dotclock;
                     else
                        Current_Delta := Target_Dotclock - Current_Clock.Dotclock;
                     end if;

                     if Current_Delta < Best_Delta
                     then
                        Best_Delta := Current_Delta;
                        Best_Clock := Current_Clock;
                        Valid      := True;
                     end if;

                     pragma Debug (Debug_Clocks, Debug.Put ("Current/Target/Best_Delta: "));
                     pragma Debug (Debug_Clocks, Debug.Put_Int64 (Current_Clock.Dotclock));
                     pragma Debug (Debug_Clocks, Debug.Put ("/"));
                     pragma Debug (Debug_Clocks, Debug.Put_Int64 (Target_Dotclock));
                     pragma Debug (Debug_Clocks, Debug.Put ("/"));
                     pragma Debug (Debug_Clocks, Debug.Put_Int64 (Best_Delta));
                     pragma Debug (Debug_Clocks, Debug.Put_Line ("."));

                  end if;
               end loop;
            end loop;
         end loop;
      end loop;

      pragma Debug (Valid,     Debug.Put_Line ("Valid clock found."));
      pragma Debug (Valid,     Debug.Put ("Best/Target/Delta: "));
      pragma Debug (Valid,     Debug.Put_Int64 (Best_Clock.Dotclock));
      pragma Debug (Valid,     Debug.Put ("/"));
      pragma Debug (Valid,     Debug.Put_Int64 (Target_Dotclock));
      pragma Debug (Valid,     Debug.Put ("/"));
      pragma Debug (Valid,     Debug.Put_Int64 (Best_Delta));
      pragma Debug (Valid,     Debug.Put_Line ("."));
      pragma Debug (not Valid, Debug.Put_Line ("No valid clock found."));

   end Calculate_Clock_Parameters;

   procedure Verify_Pineview_Parameters
      (N               : in     PNV_N_Range;
       M2              : in     PNV_M2_Range;
       P1              : in     PNV_P1_Range;
       P2              : in     PNV_P2_Range;
       Reference_Clock : in     Clock_Range;
       Current_Limits  : in     PNV_Limits_Type;
       Result          :    out PNV_Clock_Type;
       Valid           :    out Boolean)
   with
      Global => null,
      Pre => True,
      Post => True
   is
      M        : Int64;
      P        : Int64;
      VCO      : Int64;
      Dotclock : Int64;
   begin
      pragma Debug (Debug_Clocks, Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      M        := M2 + 2;
      P        := P1 * P2;
      VCO      := (Int64 (Reference_Clock) * M) / N;
      Dotclock := VCO / P;

      Valid :=
         Current_Limits.P1_Lower  <= P1  and P1  <= Current_Limits.P1_Upper  and
         (Current_Limits.P2_Fast   = P2   or P2   = Current_Limits.P2_Slow)  and
         Current_Limits.P_Lower   <= P   and P   <= Current_Limits.P_Upper   and
         Current_Limits.M2_Lower  <= M2  and M2  <= Current_Limits.M2_Upper  and
         Current_Limits.M_Lower   <= M   and M   <= Current_Limits.M_Upper   and
         PNV_VCO_Range'First      <= VCO and VCO <= PNV_VCO_Range'Last       and
         Int64 (Clock_Range'First) <= Dotclock                               and
         Dotclock <= Int64 (Clock_Range'Last);

      if Valid
      then
         Result := PNV_Clock_Type'
            (N               => N,
             M2              => M2,
             P1              => P1,
             P2              => P2,
             Reference_Clock => Reference_Clock,
             M               => M,
             P               => P,
             VCO             => VCO,
             Dotclock        => Clock_Range (Dotclock));
      else
         Result := Invalid_PNV_Clock;
      end if;
   end Verify_Pineview_Parameters;

   procedure Calculate_Pineview_Clock_Parameters
     (Display         : in     Display_Type;
      Target_Dotclock : in     Clock_Range;
      Reference_Clock : in     Clock_Range;
      Best_Clock      :    out PNV_Clock_Type;
      Valid           :    out Boolean)
   with
     Global => null,
     Pre => True,
     Post => True
   is
      Limits : constant PNV_Limits_Type :=
      (case Display is
          when LVDS   => Pineview_LVDS_Limits,
          when others => Pineview_SDVO_DAC_Limits);

      P2               : PNV_P2_Range;
      Best_Delta       : Int64 := Int64'Last;
      Current_Delta    : Int64;
      Current_Clock    : PNV_Clock_Type;
      Registers_Valid  : Boolean;
   begin
      pragma Debug (Debug_Clocks, Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Valid      := False;
      Best_Clock := Invalid_PNV_Clock;

      if Target_Dotclock <= Limits.P2_Threshold then
         P2 := Limits.P2_Slow;
      else
         P2 := Limits.P2_Fast;
      end if;

      -- Use the newer libgfxinit search order (N, then M2, then P1) and
      -- descending M2/P1 loops. Linux's Pineview code iterates M2 outermost
      -- and ascending; this only affects ties between parameter sets with
      -- the same delta.
      for N in PNV_N_Range loop
         for M2 in reverse PNV_M2_Range range Limits.M2_Lower .. Limits.M2_Upper
         loop
            pragma Loop_Invariant (True);
            for P1 in reverse PNV_P1_Range range Limits.P1_Lower .. Limits.P1_Upper
            loop
               Verify_Pineview_Parameters
                 (N               => N,
                  M2              => M2,
                  P1              => P1,
                  P2              => P2,
                  Reference_Clock => Reference_Clock,
                  Current_Limits  => Limits,
                  Result          => Current_Clock,
                  Valid           => Registers_Valid);

               if Registers_Valid
               then
                  if Current_Clock.Dotclock > Target_Dotclock
                  then
                     Current_Delta := Current_Clock.Dotclock - Target_Dotclock;
                  else
                     Current_Delta := Target_Dotclock - Current_Clock.Dotclock;
                  end if;

                  if Current_Delta < Best_Delta
                  then
                     Best_Delta := Current_Delta;
                     Best_Clock := Current_Clock;
                     Valid      := True;
                  end if;
               end if;
            end loop;
         end loop;
      end loop;
   end Calculate_Pineview_Clock_Parameters;

   procedure Program_DPLL
     (PLL      : DPLLs;
      Display  : Display_Type;
      Clk      : Clock_Type)
   with
      Global => (In_Out => Registers.Register_State),
      Pre => True,
      Post => True
   is
      FP, Encoded_P1, Encoded_P2 : Word32;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      -- FP register: N, M1, M2 encoded as (actual_value - 2)
      FP :=
         Shift_Left (Word32 (Clk.N - 2), FP_N_SHIFT)     or
         Shift_Left (Word32 (Clk.M1 - 2), FP_M1_SHIFT)   or
         Shift_Left (Word32 (Clk.M2 - 2), FP_M2_SHIFT);

      Registers.Write (FP0 (PLL), FP);
      Registers.Write (FP1 (PLL), FP);

      -- P1 encoding: bitmask (1 << (P1-1)) shifted into position
      Encoded_P1 := Shift_Left (1, Natural (Clk.P1) - 1);

      if Clk.P2 = 5 or Clk.P2 = 7
      then
         Encoded_P2 := DPLL_P2_5_OR_7;
      else
         Encoded_P2 := DPLL_P2_10_OR_14;
      end if;

      -- i945 DPLL register: no HIGH_SPEED bit, no PULSE_PHASE bits
      Registers.Write
        (Register => DPLL (PLL),
         Value    => DPLL_Mode (Display)                            or
                     DPLL_VGA_MODE_DIS                              or
                     Encoded_P2                                     or
                     Shift_Left (Encoded_P1, DPLL_P1_DIVIDER_SHIFT));
   end Program_DPLL;

   procedure Program_Pineview_DPLL
     (PLL      : DPLLs;
      Display  : Display_Type;
      Clk      : PNV_Clock_Type)
   with
      Global => (In_Out => Registers.Register_State),
      Pre => True,
      Post => True
   is
      FP, Encoded_P1, Encoded_P2 : Word32;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      -- Pineview frequency-parameter (FP) register: N is one-hot,
      -- M1 is reserved, M2 is raw.
      FP :=
         Shift_Left (Shift_Left (Word32'(1), Natural (Clk.N)), FP_N_SHIFT) or
         Shift_Left (Word32 (Clk.M2), FP_M2_SHIFT);

      Registers.Write (FP0 (PLL), FP);
      Registers.Write (FP1 (PLL), FP);

      Encoded_P1 := Shift_Left (1, Natural (Clk.P1) - 1);

      if Clk.P2 = 5 or Clk.P2 = 7
      then
         Encoded_P2 := DPLL_P2_5_OR_7;
      else
         Encoded_P2 := DPLL_P2_10_OR_14;
      end if;

      Registers.Write
        (Register => DPLL (PLL),
         Value    => DPLL_Mode (Display)                            or
                     DPLL_VGA_MODE_DIS                              or
                     Encoded_P2                                     or
                     Shift_Left (Encoded_P1, DPLL_P1_DIVIDER_SHIFT_PINEVIEW));
   end Program_Pineview_DPLL;

   procedure On
     (PLL      : in     T;
      Port_Cfg : in     Port_Config;
      Success  :    out Boolean)
   is
      Target_Clock : constant Frequency_Type := Port_Cfg.Mode.Dotclock;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Success := PLL in DPLLs;

      if Success then
         if Target_Clock <= 400_000_000 then
            if Config.CPU_Any_Pineview then
               declare
                  Clk : PNV_Clock_Type;
               begin
                  Calculate_Pineview_Clock_Parameters
                    (Display           => Port_Cfg.Display,
                     Target_Dotclock   => Target_Clock,
                     Reference_Clock   => 96_000_000,
                     Best_Clock        => Clk,
                     Valid             => Success);
                  if Success then
                     Program_Pineview_DPLL (PLL, Port_Cfg.Display, Clk);
                  end if;
               end;
            else
               declare
                  Clk : Clock_Type;
               begin
                  Calculate_Clock_Parameters
                    (Display           => Port_Cfg.Display,
                     Target_Dotclock   => Target_Clock,
                     Reference_Clock   => 96_000_000,
                     Best_Clock        => Clk,
                     Valid             => Success);
                  if Success then
                     Program_DPLL (PLL, Port_Cfg.Display, Clk);
                  end if;
               end;
            end if;
         else
            Success := False;
            pragma Debug (Debug.Put ("WARNING: Targeted clock too high: "));
            pragma Debug (Debug.Put_Int64 (Target_Clock));
            pragma Debug (Debug.Put (" > "));
            pragma Debug (Debug.Put_Int32 (400_000_000));
            pragma Debug (Debug.New_Line);
            pragma Debug (Debug.New_Line);
         end if;
      end if;

      if Success then
         Registers.Set_Mask (DPLL (PLL), DPLL_VCO_ENABLE);
         Registers.Posting_Read (DPLL (PLL));
         Time.U_Delay (150);
      end if;
   end On;

   procedure Off (PLL : T)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if PLL in DPLLs then
         Registers.Unset_Mask (DPLL (PLL), DPLL_VCO_ENABLE);
      end if;
   end Off;

   ----------------------------------------------------------------------------

   procedure Initialize
   is
   begin
      PLLs :=
        (DPLLs =>
           (Use_Count   => 0,
            Mode        => Invalid_Mode));
   end Initialize;

   procedure Alloc_Configurable
     (Port_Cfg : in     Port_Config;
      PLL      :    out T;
      Success  :    out Boolean)
   with
      Pre => True
   is
      function Config_Matches (PE : PLL_State) return Boolean
      is
      begin
         return PE.Mode = Port_Cfg.Mode;
      end Config_Matches;
   begin
      -- try to find shareable PLL
      for P in DPLLs loop
         Success := PLLs (P).Use_Count /= 0 and
                     PLLs (P).Use_Count /= Count_Range'Last and
                     Config_Matches (PLLs (P));
         if Success then
            PLL := P;
            PLLs (PLL).Use_Count := PLLs (PLL).Use_Count + 1;
            return;
         end if;
      end loop;

      -- try to find free PLL
      for P in DPLLs loop
         if PLLs (P).Use_Count = 0 then
            PLL := P;
            On (PLL, Port_Cfg, Success);
            if Success then
               PLLs (PLL) :=
                 (Use_Count   => 1,
                  Mode        => Port_Cfg.Mode);
            end if;
            return;
         end if;
      end loop;

      PLL := Invalid;
   end Alloc_Configurable;

   procedure Alloc
     (Port_Cfg : in     Port_Config;
      PLL      :    out T;
      Success  :    out Boolean)
   is
      -- On i945, DPLL-to-pipe mapping is fixed:
      -- LVDS -> Pipe B -> DPLL_B, all others -> Pipe A -> DPLL_A
      Target : constant DPLLs :=
        (if Port_Cfg.Display = LVDS then DPLL_B else DPLL_A);
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      PLL := Target;

      if PLLs (Target).Use_Count /= 0 and
         PLLs (Target).Use_Count /= Count_Range'Last and
         PLLs (Target).Mode = Port_Cfg.Mode
      then
         -- Share existing PLL with matching mode
         PLLs (Target).Use_Count := PLLs (Target).Use_Count + 1;
         Success := True;
      elsif PLLs (Target).Use_Count = 0 then
         -- Program the PLL
         On (Target, Port_Cfg, Success);
         if Success then
            PLLs (Target) :=
              (Use_Count   => 1,
               Mode        => Port_Cfg.Mode);
         end if;
      else
         PLL := Invalid;
         Success := False;
      end if;
   end Alloc;

   procedure Free (PLL : T)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if PLL in DPLLs then
         if PLLs (PLL).Use_Count /= 0 then
            PLLs (PLL).Use_Count := PLLs (PLL).Use_Count - 1;
            if PLLs (PLL).Use_Count = 0 then
               Off (PLL);
            end if;
         end if;
      end if;
   end Free;

   procedure All_Off
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for PLL in DPLLs loop
         Off (PLL);
      end loop;
   end All_Off;

   function Register_Value (PLL : T) return Word32
   is
   begin
      return (if PLL = DPLL_B then 1 else 0);
   end Register_Value;

end HW.GFX.GMA.PLLs;
