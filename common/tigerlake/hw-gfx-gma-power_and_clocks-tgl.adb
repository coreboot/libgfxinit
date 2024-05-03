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
with HW.GFX.GMA.Config;
with HW.GFX.GMA.Registers;
with HW.GFX.GMA.Connectors.TC;

use type HW.Word64;

package body HW.GFX.GMA.Power_And_Clocks.TGL is

   type Power_Domain_Types is (Power_Well, Power_DDI, Power_AUX);
   subtype PW_Domain is Power_Domain range PW1 .. PW5;
   subtype DDI_Domain is Power_Domain range DDI_A .. DDI_USBC6;
   subtype AUX_Domain is Power_Domain range AUX_A .. AUX_USBC6;
   subtype AUX_USBC_Domain is Power_Domain range AUX_USBC1 .. AUX_USBC6;
   subtype Display_Domain is Power_Domain range AUX_A .. DDI_USBC6;

   function HIP_INDEX_REG (A : AUX_USBC_Domain) return Registers.Registers_Index
   is (if A <= AUX_USBC4
       then Registers.HIP_INDEX_REG0
       else Registers.HIP_INDEX_REG1);

   function HIP_INDEX_VAL (A : AUX_USBC_Domain; Val : Word32) return Word32 is
     (case A is
      when AUX_USBC1 | AUX_USBC3 | AUX_USBC5 => Val * 2 ** 0,
      when AUX_USBC2 | AUX_USBC4 | AUX_USBC6 => Val * 2 ** 8);

   type DKL_Regs is array (AUX_USBC_Domain) of Registers.Registers_Index;
   DKL_CMN_UC_DW_27 : constant DKL_Regs := DKL_Regs'
     (AUX_USBC1 => Registers.DKL_CMN_UC_DW_27_1,
      AUX_USBC2 => Registers.DKL_CMN_UC_DW_27_2,
      AUX_USBC3 => Registers.DKL_CMN_UC_DW_27_3,
      AUX_USBC4 => Registers.DKL_CMN_UC_DW_27_4,
      AUX_USBC5 => Registers.DKL_CMN_UC_DW_27_5,
      AUX_USBC6 => Registers.DKL_CMN_UC_DW_27_6);

   ----------------------------------------------------------------------------

   function Power_Domain_Type (PD : Power_Domain) return Power_Domain_Types is
     (case PD is
      when  PW_Domain'Range => Power_Well,
      when DDI_Domain'Range => Power_DDI,
      when AUX_Domain'Range => Power_AUX);

   type Power_Well_Regs is array (Power_Domain_Types) of Registers.Registers_Index;
   PWR_CTL_BIOS : constant Power_Well_Regs :=
     (Power_Well => Registers.PWR_WELL_CTL_BIOS,
      Power_DDI  => Registers.PWR_DDI_CTL_BIOS,
      Power_AUX  => Registers.PWR_AUX_CTL_BIOS);
   PWR_CTL_DRIVER : constant Power_Well_Regs :=
     (Power_Well => Registers.PWR_WELL_CTL_DRIVER,
      Power_DDI  => Registers.PWR_DDI_CTL_DRIVER,
      Power_AUX  => Registers.PWR_AUX_CTL_DRIVER);

   function Domain_Range_Length (PD : Power_Domain) return Word32 is
     (case PD is
      when  PW_Domain'Range =>  PW_Domain'Range_Length,
      when DDI_Domain'Range => DDI_Domain'Range_Length,
      when AUX_Domain'Range => AUX_Domain'Range_Length);

   function Domain_First (PD : Power_Domain) return Power_Domain is
     (case PD is
      when  PW_Domain'Range =>  PW_Domain'First,
      when DDI_Domain'Range => DDI_Domain'First,
      when AUX_Domain'Range => AUX_Domain'First);

   function Domain_Index (PD : Power_Domain) return Natural
   with
      SPARK_Mode => Off,
      Post => Domain_Index'Result < Domain_Range_Length (PD)
   is
   begin
     return Power_Domain'Pos (PD) - Power_Domain'Pos (Domain_First (PD));
   end Domain_Index;

   function Power_Request_Mask (PD : Power_Domain) return Word32 is
    (2 * 2 ** (2 * Domain_Index (PD)));

   function Power_State_Mask (PD : Power_Domain) return Word32 is
     (1 * 2 ** (2 * Domain_Index (PD)));

   ----------------------------------------------------------------------------

   FUSE_STATUS_PG0_DIST_STATUS   : constant := 1 * 2 ** 27;
   type Power_Well_Values is array (PW_Domain) of Word32;
   FUSE_STATUS_PGx_DIST_STATUS : constant Power_Well_Values :=
     (PW1   => 1 * 2 ** 26,
      PW2   => 1 * 2 ** 25,
      PW3   => 1 * 2 ** 24,
      PW4   => 1 * 2 ** 23,
      PW5   => 1 * 2 ** 22);

   ----------------------------------------------------------------------------

   type AUX_CTL_Array is array (AUX_USBC_Domain) of Registers.Registers_Index;
   AUX_CTL_Regs : constant AUX_CTL_Array :=
      AUX_CTL_Array'
     (AUX_USBC1 => Registers.DDI_AUX_CTL_USBC1,
      AUX_USBC2 => Registers.DDI_AUX_CTL_USBC2,
      AUX_USBC3 => Registers.DDI_AUX_CTL_USBC3,
      AUX_USBC4 => Registers.DDI_AUX_CTL_USBC4,
      AUX_USBC5 => Registers.DDI_AUX_CTL_USBC5,
      AUX_USBC6 => Registers.DDI_AUX_CTL_USBC6);

   function Aux_To_Port (PD : AUX_USBC_Domain) return USBC_Port is
     (case PD is
      when AUX_USBC1 => DDI_TC1,
      when AUX_USBC2 => DDI_TC2,
      when AUX_USBC3 => DDI_TC3,
      when AUX_USBC4 => DDI_TC4,
      when AUX_USBC5 => DDI_TC5,
      when AUX_USBC6 => DDI_TC6);

   procedure Pre_On (PD : Power_Domain)
   is
      DP_AUX_CH_CTL_TBT_IO : constant := 1 * 2 ** 11;
   begin
      if PD in AUX_USBC_Domain then
         -- Disable TBT IO mode for AUX
         Registers.Unset_Mask
           (Register => AUX_CTL_Regs (PD),
            Mask     => DP_AUX_CH_CTL_TBT_IO);
      elsif PD = PW1 then
         Registers.Wait_Set_Mask
           (Register => Registers.FUSE_STATUS,
            Mask     => FUSE_STATUS_PG0_DIST_STATUS);
      end if;
   end Pre_On;

   procedure Pre_Off (PD : Power_Domain) is
   begin
      if PD in AUX_USBC_Domain then
         Connectors.TC.Disconnect (Aux_To_Port (PD));
      end if;
   end Pre_Off;

   procedure Post_On (PD : Power_Domain)
   is
      DKL_CMN_UC_DW_27_UC_HEALTH : constant := 1 * 2 ** 15;
   begin
      if PD in PW_Domain then
         Registers.Wait_Set_Mask (Registers.FUSE_STATUS,
                                  FUSE_STATUS_PGx_DIST_STATUS (PD));
      elsif PD in AUX_USBC_Domain then
         Registers.Write (HIP_INDEX_REG (PD), HIP_INDEX_VAL (PD, 2));
         Registers.Wait_Set_Mask
            (Register => DKL_CMN_UC_DW_27 (PD),
             Mask     => DKL_CMN_UC_DW_27_UC_HEALTH);
      end if;
   end Post_On;

   procedure PD_On (PD : Power_Domain)
   is
      Ctl1, Ctl2 : Word32;
      PD_Type : constant Power_Domain_Types := Power_Domain_Type (PD);
      Success : Boolean;
      DP_AUX_CH_CTL_TBT_IO : constant := 1 * 2 ** 11;
   begin
      Pre_On (PD);

      Registers.Read (PWR_CTL_BIOS (PD_Type), Ctl1);
      Registers.Read (PWR_CTL_DRIVER (PD_Type), Ctl2);

      if ((Ctl1 or Ctl2) and Power_Request_Mask (PD)) = 0 then
         Registers.Wait_Unset_Mask
           (Register => PWR_CTL_DRIVER (PD_Type),
            Mask     => Power_State_Mask (PD));
      end if;

      if (Ctl2 and Power_Request_Mask (PD)) = 0 then
         Registers.Set_Mask (PWR_CTL_DRIVER (PD_Type), Power_Request_Mask (PD));
         Registers.Wait_Set_Mask
           (Register => PWR_CTL_DRIVER (PD_Type),
            Mask => Power_State_Mask (PD),
            Success => Success);

         if not Success then
            Debug.Put_Line ("Failed to enable power domain!");
            return;
         end if;
      end if;

      Post_On (PD);
   end PD_On;

   procedure PD_Off (PD : Power_Domain)
   is
      Ctl1, Ctl2 : Word32;
      PD_Type : constant Power_Domain_Types := Power_Domain_Type (PD);
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      Registers.Read (PWR_CTL_BIOS (PD_Type), Ctl1);
      Registers.Read (PWR_CTL_DRIVER (PD_Type), Ctl2);

      -- PW1 is "always-on"
      if PD = PW1 then
         return;
      end if;

      if ((Ctl1 or Ctl2) and Power_Request_Mask (PD)) /= 0 then
         Registers.Wait_Set_Mask
           (Register => PWR_CTL_DRIVER (PD_Type),
            Mask     => Power_State_Mask (PD));
      end if;

      Pre_Off (PD);

      if (Ctl2 and Power_Request_Mask (PD)) /= 0 then
         Registers.Unset_Mask (PWR_CTL_DRIVER (PD_Type), Power_Request_Mask (PD));
         Registers.Unset_Mask (PWR_CTL_BIOS (PD_Type), Power_Request_Mask (PD));
      end if;
   end PD_Off;

   function Need_PD (PD : Power_Domain; Configs : Pipe_Configs) return Boolean
   is
      generic
         type T is new Port_Type;
         First : Port_Type := Port_Type (T'First);
         Last  : Port_Type := Port_Type (T'Last);
      function Any_Port_Of return Boolean;
      function Any_Port_Of return Boolean is
        (Configs (Primary).Port   in First .. Last or else
         Configs (Secondary).Port in First .. Last or else
         Configs (Tertiary).Port  in First .. Last);

      function Any_Port_Combo is new Any_Port_Of (T => Combo_Port_Type);
      function Any_Port_USBC  is new Any_Port_Of (T =>  USBC_Port_Type);

      function Num_Active_Pipes return Natural is
         Count : Natural := 0;
      begin
         for I in Pipe_Index loop
            if Configs (I).Port /= Disabled then
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end Num_Active_Pipes;

      function PW_Need_PD (PD : PW_Domain) return Boolean is
        (case PD is
            when PW1 |
                 PW2 => Num_Active_Pipes >= 1 or else Any_Port_USBC,
            when PW3 => Num_Active_Pipes >= 2 or else Any_Port_USBC,
            when PW4 => Num_Active_Pipes >= 3,
            when PW5 => False);

      function Port_Need_PD (PD : Display_Domain; Port : Port_Type) return Boolean is
        (case PD is
            when DDI_A | AUX_A         => Port in DP1 | HDMI1 | eDP,
            when DDI_B | AUX_B         => Port in DP2 | HDMI2,
            when DDI_C | AUX_C         => Port in DP3 | HDMI3,
            when DDI_USBC1 | AUX_USBC1 => Port in USBC1_DP | USBC1_HDMI,
            when DDI_USBC2 | AUX_USBC2 => Port in USBC2_DP | USBC2_HDMI,
            when DDI_USBC3 | AUX_USBC3 => Port in USBC3_DP | USBC3_HDMI,
            when DDI_USBC4 | AUX_USBC4 => Port in USBC4_DP | USBC4_HDMI,
            when DDI_USBC5 | AUX_USBC5 => Port in USBC5_DP | USBC5_HDMI,
            when DDI_USBC6 | AUX_USBC6 => Port in USBC6_DP | USBC6_HDMI);

      function Display_Need_PD (PD : Display_Domain) return Boolean is
        (Port_Need_PD (PD, Configs (Primary).Port)   or else
         Port_Need_PD (PD, Configs (Secondary).Port) or else
         Port_Need_PD (PD, Configs (Tertiary).Port));
   begin
      return (case PD is
         when PW_Domain      => PW_Need_PD (PD),
         when Display_Domain => Display_Need_PD (PD));
   end Need_PD;

   procedure Aux_Off is
   begin
      for AUX in reverse AUX_USBC1 .. AUX_USBC6 loop
         PD_Off (AUX);
      end loop;
   end Aux_off;

   procedure Init_Power is
   begin
      PD_On (PW1);
   end Init_Power;

   procedure Power_Set_To (Configs : Pipe_Configs) is
   begin
      for PD in reverse Power_Domain loop
         if not Need_PD (PD, Configs) then
            PD_Off (PD);
         end if;
      end loop;

      for PD in Power_Domain loop
         if Need_PD (PD, Configs) then
            PD_On (PD);
         end if;
      end loop;
   end Power_Set_To;

   procedure Power_Up (Old_Configs, New_Configs : Pipe_Configs)
   is
      Success : Boolean;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Connectors.TC.TC_Cold_Request (Connectors.TC.Block, Success);
      if not Success then
         Debug.Put_Line ("Failed to unblock TCCOLD");
      end if;

      for PD in Power_Domain loop
        if not Need_PD (PD, Old_Configs) and Need_PD (PD, New_Configs) then
           PD_On (PD);
        end if;
      end loop;
   end Power_Up;

   procedure Power_Down (Old_Configs, Tmp_Configs, New_Configs : Pipe_Configs)
   is
   begin
      for PD in reverse Power_Domain loop
        if (Need_PD (PD, Old_Configs) or Need_PD (PD, Tmp_Configs)) and
           not Need_PD (PD, New_Configs)
        then
           PD_Off (PD);
        end if;
      end loop;
   end Power_Down;

   procedure All_Off is
   begin
      for AUX in reverse AUX_A .. AUX_USBC6 loop
         PD_Off (AUX);
      end loop;

      for DDI in reverse DDI_A .. DDI_USBC6 loop
         PD_Off (DDI);
      end loop;

      for PW in reverse PW1 .. PW5 loop
         PD_Off (PW);
      end loop;
   end All_Off;

end HW.GFX.GMA.Power_And_Clocks.TGL;
