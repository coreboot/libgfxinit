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
with HW.GFX.GMA.Config_Helpers;
with HW.GFX.GMA.Connectors.TC.Ownership;

package body HW.GFX.GMA.Power_Domains_Common is

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

   FUSE_STATUS_PG0_DIST_STATUS : constant := 1 * 2 ** 27;

   ----------------------------------------------------------------------------

   type Domain_Types is (Power_Well, Power_DDI, Power_AUX);

   function Domain_Type (PD : Power_Domain) return Domain_Types
   is
     (case PD is
         when PW_Domain'Range  => Power_Well,
         when DDI_Domain'Range => Power_DDI,
         when AUX_Domain'Range => Power_AUX);

   type Power_Well_Regs is array (Domain_Types) of Registers.Registers_Index;
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
         when PW_Domain'Range    => PW_State_Mask (PD),
         when DDI_Domain'Range   => 1 * 2 ** (2 * DDI_Index (PD)),
         when AUX_Domain'Range   => 1 * 2 ** (2 * AUX_Index (PD)));

   function Power_Request_Mask (PD : Power_Domain) return Word32 is
   begin
      return Shift_Left (Power_State_Mask (PD), 1);
   end Power_Request_Mask;

   ----------------------------------------------------------------------------

   function HIP_INDEX_REG (Aux : AUX_Domain) return Registers.Registers_Index
   is
     (if Aux <= AUX_USBC4
      then Registers.HIP_INDEX_REG0
      else Registers.HIP_INDEX_REG1);

   function HIP_INDEX_VAL (Aux : AUX_USBC_Domain; Val : Word32) return Word32
   is
     (Val * 2 ** (8 * (AUX_USBC_Index (Aux) mod 4)));

   DKL_CMN_UC_DW_27 : constant array (USBC_Port) of Registers.Registers_Index :=
     (DDI_TC1  => Registers.DKL_CMN_UC_DW_27_1,
      DDI_TC2  => Registers.DKL_CMN_UC_DW_27_2,
      DDI_TC3  => Registers.DKL_CMN_UC_DW_27_3,
      DDI_TC4  => Registers.DKL_CMN_UC_DW_27_4,
      DDI_TC5  => Registers.DKL_CMN_UC_DW_27_5,
      DDI_TC6  => Registers.DKL_CMN_UC_DW_27_6);

   ----------------------------------------------------------------------------

   AUX_CTL_Regs : constant array (USBC_Port) of Registers.Registers_Index :=
     (DDI_TC1 => Registers.DDI_AUX_CTL_USBC1,
      DDI_TC2 => Registers.DDI_AUX_CTL_USBC2,
      DDI_TC3 => Registers.DDI_AUX_CTL_USBC3,
      DDI_TC4 => Registers.DDI_AUX_CTL_USBC4,
      DDI_TC5 => Registers.DDI_AUX_CTL_USBC5,
      DDI_TC6 => Registers.DDI_AUX_CTL_USBC6);

   ----------------------------------------------------------------------------

   procedure Pre_PD_On (PD : in Power_Domain; Success : out Boolean)
   is
      DP_AUX_CH_CTL_TBT_IO : constant := 1 * 2 ** 11;
   begin
      if PD in AUX_USBC_Domain then
         -- Disable TBT IO mode for AUX
         Registers.Unset_Mask
           (Register => AUX_CTL_Regs (To_GPU_Port (PD)),
            Mask     => DP_AUX_CH_CTL_TBT_IO);
         Connectors.TC.Ownership.Claimed (To_GPU_Port (PD), Success);
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
           (Register => DKL_CMN_UC_DW_27 (To_GPU_Port (PD)),
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
         Connectors.TC.Ownership.Disconnect (To_GPU_Port (PD));
      end if;
   end Pre_PD_Off;

   procedure PD_On (PD : Power_Domain)
   is
      Ctl1, Ctl2 : Word32;
      PD_Type : constant Domain_Types := Domain_Type (PD);
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
      PD_Type : constant Domain_Types := Domain_Type (PD);
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

   ----------------------------------------------------------------------------

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
            when Port_Domain'Range  => Any_Port_Is (To_GPU_Port (PD)));
   end Need_PD;

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

   procedure All_Off is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for PD in reverse Power_Domain loop
         PD_Off (PD);
      end loop;
   end All_Off;

end HW.GFX.GMA.Power_Domains_Common;
