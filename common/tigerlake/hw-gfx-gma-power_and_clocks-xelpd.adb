-- (C) 2022 Google, LLC
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

package body HW.GFX.GMA.Power_And_Clocks.XELPD is

   -- Comment lifted from i915:
   -- XE_LPD Power Domains
   --
   -- Previous platforms required that PG(n-1) be enabled before PG(n).  That
   -- dependency chain turns into a dependency tree on XE_LPD:
   --
   --       PG0
   --        |
   --     --PG1--
   --    /       \
   --  PGA     --PG2--
   --         /   |   \
   --       PGB  PGC  PGD
   --
   -- Power wells must be enabled from top to bottom and disabled from bottom
   -- to top.  This allows pipes to be power gated independently.

   type Power_Domain_Types is (Power_Well, Power_DDI, Power_AUX);
   subtype PW_Domain       is Power_Domain range PW1 .. PWD;
   subtype DDI_Domain      is Power_Domain range DDI_A .. DDI_USBC4;
   subtype AUX_Domain      is Power_Domain range AUX_A .. AUX_USBC4;
   subtype AUX_USBC_Domain is Power_Domain range AUX_USBC1 .. AUX_USBC4;
   subtype Display_Domain  is Power_Domain range AUX_A .. DDI_USBC4;

   function Power_Domain_Type (PD : Power_Domain) return Power_Domain_Types is
     (if    PD in PW_Domain  then Power_Well
      elsif PD in DDI_Domain then Power_DDI
      else                        Power_AUX);

   HIP_INDEX_REG : constant Registers.Registers_Index := Registers.HIP_INDEX_REG0;
   function HIP_INDEX_VAL (A : AUX_USBC_Domain; Val : Word32) return Word32 is
     (case A is
      when AUX_USBC1 | AUX_USBC3 => Val,
      when AUX_USBC2 | AUX_USBC4 => Shift_Left (Val, 8));

   type DKL_Regs is array (AUX_USBC_Domain) of Registers.Registers_Index;
   DKL_CMN_UC_DW_27 : constant DKL_Regs := DKL_Regs'
     (AUX_USBC1 => Registers.DKL_CMN_UC_DW_27_1,
      AUX_USBC2 => Registers.DKL_CMN_UC_DW_27_2,
      AUX_USBC3 => Registers.DKL_CMN_UC_DW_27_3,
      AUX_USBC4 => Registers.DKL_CMN_UC_DW_27_4);

   type Power_Well_Regs is array (Power_Domain_Types) of Registers.Registers_Index;
   PWR_CTL_BIOS : constant Power_Well_Regs :=
     (Power_Well => Registers.PWR_WELL_CTL_BIOS,
      Power_DDI  => Registers.PWR_DDI_CTL_BIOS,
      Power_AUX  => Registers.PWR_AUX_CTL_BIOS);
   PWR_CTL_DRIVER : constant Power_Well_Regs :=
     (Power_Well => Registers.PWR_WELL_CTL_DRIVER,
      Power_DDI  => Registers.PWR_DDI_CTL_DRIVER,
      Power_AUX  => Registers.PWR_AUX_CTL_DRIVER);

   FUSE_STATUS_PG0_DIST_STATUS                    : constant := 1 * 2 ** 27;
   type Power_Well_Values is array (PW_Domain) of Word32;
   FUSE_STATUS_PGx_DIST_STATUS : constant Power_Well_Values :=
     (PW1   => 1 * 2 ** 26,
      PW2   => 1 * 2 ** 25,
      PWA   => 1 * 2 ** 21,
      PWB   => 1 * 2 ** 20,
      PWC   => 1 * 2 ** 19,
      PWD   => 1 * 2 ** 18);

   function Power_State_Mask (PD : Power_Domain) return Word32 is
   begin
      return
         2 ** (2 * (case PD is
              when PW1       => 0,
              when PW2       => 1,
              when PWA       => 5,
              when PWB       => 6,
              when PWC       => 7,
              when PWD       => 8,
              when AUX_A     => 0,
              when AUX_B     => 1,
              when AUX_USBC1 => 5,
              when AUX_USBC2 => 6,
              when AUX_USBC3 => 7,
              when AUX_USBC4 => 8,
              when DDI_A     => 0,
              when DDI_B     => 1,
              when DDI_USBC1 => 5,
              when DDI_USBC2 => 6,
              when DDI_USBC3 => 7,
              when DDI_USBC4 => 8));
   end Power_State_Mask;

   function Power_Request_Mask (PD : Power_Domain) return Word32 is
   begin
      return Shift_Left (Power_State_Mask (PD), 1);
   end Power_Request_Mask;

   type AUX_CTL_Array is array (AUX_USBC_Domain) of Registers.Registers_Index;
   AUX_CTL_Regs : constant AUX_CTL_Array :=
      AUX_CTL_Array'
     (AUX_USBC1 => Registers.DDI_AUX_CTL_USBC1,
      AUX_USBC2 => Registers.DDI_AUX_CTL_USBC2,
      AUX_USBC3 => Registers.DDI_AUX_CTL_USBC3,
      AUX_USBC4 => Registers.DDI_AUX_CTL_USBC4);

   procedure Pre_On (PD : Power_Domain)
   is
      DP_AUX_CH_CTL_TBT_IO : constant := 1 * 2 ** 11;
   begin
      if PD in AUX_USBC_Domain then
         Registers.Unset_Mask
           (Register => AUX_CTL_Regs (PD),
            Mask     => DP_AUX_CH_CTL_TBT_IO);
      elsif PD = PW1 then
         Registers.Set_Mask
           (Register => Registers.GEN8_CHICKEN_DCPR_1,
            Mask     => 1 * 2 ** 15); -- DISABLE_FLR_SRC
         Registers.Wait_Set_Mask
           (Register => Registers.FUSE_STATUS,
            Mask     => FUSE_STATUS_PG0_DIST_STATUS);
      end if;
   end Pre_On;

   procedure Post_On (PD : Power_Domain)
   is
      DKL_CMN_UC_DW_27_UC_HEALTH : constant := 1 * 2 ** 15;
   begin
      if PD in PW_Domain then
         Registers.Wait_Set_Mask (Registers.FUSE_STATUS,
                                  FUSE_STATUS_PGx_DIST_STATUS (PD));
      elsif PD in AUX_USBC_Domain then
         Registers.Write (HIP_INDEX_REG, HIP_INDEX_VAL (PD, 2));
         Registers.Wait_Set_Mask
            (Register => DKL_CMN_UC_DW_27 (PD),
             Mask     => DKL_CMN_UC_DW_27_UC_HEALTH);
      end if;
   end Post_On;

   function Aux_To_Port (PD : AUX_USBC_Domain) return USBC_Port
   is (case PD is
       when AUX_USBC1 => DDI_TC1,
       when AUX_USBC2 => DDI_TC2,
       when AUX_USBC3 => DDI_TC3,
       when AUX_USBC4 => DDI_TC4);

   procedure Pre_Off (PD : Power_Domain) is
   begin
      if PD in AUX_USBC_Domain then
         Connectors.TC.Disconnect (Aux_To_Port (PD));
      end if;
   end Pre_Off;

   procedure PD_On (PD : Power_Domain) is
      PD_Type : constant Power_Domain_Types := Power_Domain_Type (PD);
      Ctl1, Ctl2 : Word32;
      Success : Boolean;
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
            if PD in AUX_USBC_Domain then
               pragma Debug (Debug.Put_Line ("AUX timeout expected."));
            else
               Debug.Put_Line ("Failed to enable power domain!");
               return;
            end if;
         end if;
      end if;

      Post_On (PD);
   end PD_On;

   procedure PD_Off (PD : Power_Domain)
   is
      PD_Type : constant Power_Domain_Types := Power_Domain_Type (PD);
      Ctl1, Ctl2 : Word32;
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
            when PW1 => True,
            when PW2 => Any_Port_USBC,
            when PWA => Num_Active_Pipes >= 1,
            when PWB => Num_Active_Pipes >= 2,
			when PWC => Num_Active_Pipes >= 3,
            when PWD => Num_Active_Pipes >= 4);

      function Port_Need_PD (PD : Display_Domain; Port : Port_Type) return Boolean is
        (case PD is
            when DDI_A | AUX_A         => Port in DP1 | HDMI1 | eDP,
            when DDI_B | AUX_B         => Port in DP2 | HDMI2,
            when DDI_USBC1 | AUX_USBC1 => Port in USBC1_DP | USBC1_HDMI,
            when DDI_USBC2 | AUX_USBC2 => Port in USBC2_DP | USBC2_HDMI,
            when DDI_USBC3 | AUX_USBC3 => Port in USBC3_DP | USBC3_HDMI,
            when DDI_USBC4 | AUX_USBC4 => Port in USBC4_DP | USBC4_HDMI);

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
      for AUX in reverse AUX_USBC1 .. AUX_USBC4 loop
         PD_Off (AUX);
      end loop;
      PD_Off (AUX_B);
      PD_Off (AUX_A);
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

   procedure Power_Up (Old_Configs, New_Configs : Pipe_Configs) is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

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
      for AUX in reverse AUX_A .. AUX_USBC4 loop
         PD_Off (AUX);
      end loop;

      for DDI in reverse DDI_A .. DDI_USBC4 loop
         PD_Off (DDI);
      end loop;

      for PW in reverse PW1 .. PWD loop
         PD_Off (PW);
      end loop;
   end All_Off;

   procedure Power_Up_Aux is
   begin
      for Aux in AUX_Domain loop
         PD_On (Aux);
      end loop;
   end Power_Up_Aux;

end HW.GFX.GMA.Power_And_Clocks.XELPD;
