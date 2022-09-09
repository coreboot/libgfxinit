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
with HW.GFX.GMA.Registers;

package body HW.GFX.GMA.Combo_Phy is

   PORT_PCS_DW1_DCC_MODE_SELECT_MASK             : constant := 3 * 2 ** 20;
   PORT_PCS_DW1_DCC_MODE_SELECT_CONTINUOUS       : constant := 3 * 2 ** 20;
   PORT_COMP_DW0_COMP_INIT                       : constant := 1 * 2 ** 31;
   ICL_PORT_TX_DW8_ODCC_CLK_DIV_SEL_DIV2         : constant := 1 * 2 ** 29;
   PORT_CL_DW5_POWER_DOWN_ENABLE                 : constant := 1 * 2 ** 4;
   PHY_MISC_DE_TO_IO_COMP_PWR_DOWN               : constant := 1 * 2 ** 23;
   PORT_TX_DW8_ODCC_DIV_SEL_MASK                 : constant := 3 * 2 ** 29;
   PORT_TX_DW8_ODCC_CLKSEL                       : constant := 1 * 2 ** 31;
   PORT_COMP_DW8_IREFGEN                         : constant := 1 * 2 ** 24;
   PORT_COMP_DW1_REF_MASK                        : constant := 16#ff_00ff#;

   type Combo_Phy is (DDI_A, DDI_B, DDI_C);

   type Phy_Regs_Record is record
      PHY_MISC         : Registers.Registers_Index;
      PORT_CL_DW5      : Registers.Registers_Index;
      PORT_COMP_DW0    : Registers.Registers_Index;
      PORT_COMP_DW1    : Registers.Registers_Index;
      PORT_COMP_DW3    : Registers.Registers_Index;
      PORT_TX_DW8_LN0  : Registers.Registers_Index;
      PORT_TX_DW8_GRP  : Registers.Registers_Index;
      PORT_PCS_DW1_LN0 : Registers.Registers_Index;
      PORT_PCS_DW1_GRP : Registers.Registers_Index;
      PORT_COMP_DW8    : Registers.Registers_Index;
      PORT_COMP_DW9     : Registers.Registers_Index;
      PORT_COMP_DW10    : Registers.Registers_Index;
   end record;

   type Combo_Phy_Regs is array (Combo_Phy) of Phy_Regs_Record;
   Phy_Regs : constant Combo_Phy_Regs := Combo_Phy_Regs'
     (DDI_A => Phy_Regs_Record'
       (PHY_MISC         => Registers.PHY_MISC_A,
        PORT_CL_DW5      => Registers.PORT_CL_DW5_A,
        PORT_COMP_DW0    => Registers.PORT_COMP_DW0_A,
        PORT_COMP_DW1    => Registers.PORT_COMP_DW1_A,
        PORT_COMP_DW3    => Registers.PORT_COMP_DW3_A,
        PORT_TX_DW8_LN0  => Registers.PORT_TX_DW8_LN0_A,
        PORT_TX_DW8_GRP  => Registers.PORT_TX_DW8_GRP_A,
        PORT_PCS_DW1_LN0 => Registers.PORT_PCS_DW1_LN0_A,
        PORT_PCS_DW1_GRP => Registers.PORT_PCS_DW1_GRP_A,
        PORT_COMP_DW8    => Registers.PORT_COMP_DW8_A,
        PORT_COMP_DW9    => Registers.PORT_COMP_DW9_A,
        PORT_COMP_DW10   => Registers.PORT_COMP_DW10_A),
      DDI_B => Phy_Regs_Record'
       (PHY_MISC         => Registers.PHY_MISC_B,
        PORT_CL_DW5      => Registers.PORT_CL_DW5_B,
        PORT_COMP_DW0    => Registers.PORT_COMP_DW0_B,
        PORT_COMP_DW1    => Registers.PORT_COMP_DW1_B,
        PORT_COMP_DW3    => Registers.PORT_COMP_DW3_B,
        PORT_TX_DW8_LN0  => Registers.PORT_TX_DW8_LN0_B,
        PORT_TX_DW8_GRP  => Registers.PORT_TX_DW8_GRP_B,
        PORT_PCS_DW1_LN0 => Registers.PORT_PCS_DW1_LN0_B,
        PORT_PCS_DW1_GRP => Registers.PORT_PCS_DW1_GRP_B,
        PORT_COMP_DW8    => Registers.PORT_COMP_DW8_B,
        PORT_COMP_DW9    => Registers.PORT_COMP_DW9_B,
        PORT_COMP_DW10   => Registers.PORT_COMP_DW10_B),
      DDI_C => Phy_Regs_Record'
       (PHY_MISC         => Registers.PHY_MISC_C,
        PORT_CL_DW5      => Registers.PORT_CL_DW5_C,
        PORT_COMP_DW0    => Registers.PORT_COMP_DW0_C,
        PORT_COMP_DW1    => Registers.PORT_COMP_DW1_C,
        PORT_COMP_DW3    => Registers.PORT_COMP_DW3_C,
        PORT_TX_DW8_LN0  => Registers.PORT_TX_DW8_LN0_C,
        PORT_TX_DW8_GRP  => Registers.PORT_TX_DW8_GRP_C,
        PORT_PCS_DW1_LN0 => Registers.PORT_PCS_DW1_LN0_C,
        PORT_PCS_DW1_GRP => Registers.PORT_PCS_DW1_GRP_C,
        PORT_COMP_DW8    => Registers.PORT_COMP_DW8_C,
        PORT_COMP_DW9    => Registers.PORT_COMP_DW9_C,
        PORT_COMP_DW10   => Registers.PORT_COMP_DW10_C));

   procedure Config_DCC_SusClk (Phy : Combo_Phy)
   is
      DW8 : Word32;
      DW1 : Word32;
   begin
      -- Read from lane 0 and write to the group
      Registers.Read (Phy_Regs (Phy).PORT_TX_DW8_LN0, DW8);
      DW8 := DW8 and not PORT_TX_DW8_ODCC_DIV_SEL_MASK;
      DW8 := DW8 or PORT_TX_DW8_ODCC_CLKSEL;
      DW8 := DW8 or ICL_PORT_TX_DW8_ODCC_CLK_DIV_SEL_DIV2;
      Registers.Write (Phy_Regs (Phy).PORT_TX_DW8_GRP, DW8);

      Registers.Read (Phy_Regs (Phy).PORT_PCS_DW1_LN0, DW1);
      DW1 := DW1 and not PORT_PCS_DW1_DCC_MODE_SELECT_MASK;
      DW1 := DW1 or PORT_PCS_DW1_DCC_MODE_SELECT_CONTINUOUS;
      Registers.Write (Phy_Regs (Phy).PORT_PCS_DW1_GRP, DW1);
   end Config_DCC_SusClk;

   procedure Config_Procmon_Reference (Phy : Combo_Phy)
   is
      type Procmon_Voltage is (VOLT_0_85, VOLT_0_95, VOLT_1_05);
      type Procmon_Process is (DOT0, DOT1);
      type Procmon_References is record
         DW1     : Word32;
         DW9     : Word32;
         DW10    : Word32;
      end record;

      DOT0_VOLT_0_85 : constant Procmon_References :=
        (DW1     => 16#0000_0000#,
         DW9     => 16#62ab_67bb#,
         DW10    => 16#5191_4f96#);
      DOT0_VOLT_0_95 : constant Procmon_References :=
        (DW1     => 16#0000_0000#,
         DW9     => 16#86e1_72c7#,
         DW10    => 16#77ca_5eab#);
      DOT0_VOLT_1_05 : constant Procmon_References :=
        (DW1     => 16#0000_0000#,
         DW9     => 16#98fa_82dd#,
         DW10    => 16#89e4_6dc1#);
      DOT1_VOLT_0_95 : constant Procmon_References :=
        (DW1     => 16#0000_0000#,
         DW9     => 16#93f8_7fe1#,
         DW10    => 16#8ae8_71c5#);
      DOT1_VOLT_1_05 : constant Procmon_References :=
        (DW1     => 16#0044_0000#,
         DW9     => 16#9a00_ab25#,
         DW10    => 16#8ae3_8ff1#);
      procedure Read_DW3 (Phy : Combo_Phy; References : out Procmon_References)
      is
         DW3, Tmp : Word32;
         Process : Procmon_Process;
         Voltage : Procmon_Voltage;
         PROCESS_MASK : constant := 7 * 2 ** 26;
         VOLTAGE_MASK : constant := 3 * 2 ** 24;
      begin
         Registers.Read (Phy_Regs (Phy).PORT_COMP_DW3, DW3);

         Tmp := Shift_Right (DW3 and VOLTAGE_MASK, 24);
         case (Tmp) is
            when 0 => Voltage := VOLT_0_85;
            when 1 => Voltage := VOLT_0_95;
            when 2 => Voltage := VOLT_1_05;
            when others => Voltage := VOLT_0_85;
         end case;

         Tmp := Shift_Right (DW3 and PROCESS_MASK, 26);
         case (Tmp) is
            when 0 => Process := DOT0;
            when 1 => Process := DOT1;
            when others => Process := DOT0;
         end case;

         if Process = DOT0 then
            case (Voltage) is
               when VOLT_0_85 => References := DOT0_VOLT_0_85;
               when VOLT_0_95 => References := DOT0_VOLT_0_95;
               when VOLT_1_05 =>  References := DOT0_VOLT_1_05;
            end case;
         else
            case (Voltage) is
               -- [DOT1, VOLT_0_85] is actually an invalid combination
	       when VOLT_0_95 | VOLT_0_85 => References := DOT1_VOLT_0_95;
               when VOLT_1_05 =>  References := DOT1_VOLT_1_05;
            end case;
         end if;
      end Read_DW3;

      References : Procmon_References;
   begin
      Read_DW3 (Phy, References);
      Registers.Unset_And_Set_Mask (Register => Phy_Regs (Phy).PORT_COMP_DW1,
                                    Mask_Unset => PORT_COMP_DW1_REF_MASK,
                                    Mask_Set => References.DW1);
      Registers.Write (Phy_Regs (Phy).PORT_COMP_DW9 , References.DW9);
      Registers.Write (Phy_Regs (Phy).PORT_COMP_DW10, References.DW10);
   end Config_Procmon_Reference;

   function Phy_Is_Master (Phy : Combo_Phy) return Boolean is
   begin
      case Phy is
         when DDI_A => return True;
         when others => return False;
      end case;
   end Phy_Is_Master;

   procedure Initialize
   is
      Was_Enabled : Boolean;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
       -- Initialize all combo PHYs with Combo PHY DDI Buffer Combo PHY Init Sequence
      for Phy in Combo_Phy'range loop
         Registers.Is_Set_Mask (
            Phy_Regs (Phy).PORT_COMP_DW0,
            PORT_COMP_DW0_COMP_INIT,
            Was_Enabled);

         if not Was_Enabled then
            Config_DCC_SusClk (Phy);

            Registers.Unset_Mask
              (Phy_Regs (Phy).PHY_MISC,
               PHY_MISC_DE_TO_IO_COMP_PWR_DOWN);

            Config_Procmon_Reference (Phy);

            if Phy_Is_Master (Phy) then
               Registers.Set_Mask (Register => Phy_Regs (Phy).PORT_COMP_DW8,
                                   Mask => PORT_COMP_DW8_IREFGEN);
            end if;

            Registers.Set_Mask (Register => Phy_Regs (Phy).PORT_COMP_DW0,
                                Mask => PORT_COMP_DW0_COMP_INIT);

            Registers.Set_Mask (Register => Phy_Regs (Phy).PORT_CL_DW5,
                                Mask => PORT_CL_DW5_POWER_DOWN_ENABLE);
         end if;
      end loop;
   end Initialize;

   procedure All_Off is
   begin
      for Phy in Combo_Phy'range loop
         Registers.Set_Mask (Phy_Regs (Phy).PHY_MISC,
                             PHY_MISC_DE_TO_IO_COMP_PWR_DOWN);

         Registers.Unset_Mask (Phy_Regs (Phy).PORT_COMP_DW0,
                               PORT_COMP_DW0_COMP_INIT);
      end loop;
   end All_Off;

end HW.GFX.GMA.Combo_Phy;
