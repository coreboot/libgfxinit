--
-- Copyright (C) Google, LLC
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
with HW.GFX.GMA.Config_Helpers;
with HW.GFX.GMA.Connectors.TC;
with HW.GFX.GMA.Power_And_Clocks;

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Port_Detect
is
   type Digital_Port_Value is array (Digital_Port) of Word32;

   function SDEISR_MASK (Port : Active_Port_Type) return Word32
   is (case Port is
       when eDP         => 16#1_0000#,
       when DP1 | HDMI1 => 16#1_0000#,
       when DP2 | HDMI2 => 16#2_0000#,
       when DP3 | HDMI3 => 16#4_0000#,
       when others      => 0);

   function DE_HPD_ISR_MASK (Port : Active_Port_Type) return Word32
   is (case Port is
       when USBC1_DP | USBC1_HDMI => 16#01_0000#,
       when USBC2_DP | USBC2_HDMI => 16#02_0000#,
       when USBC3_DP | USBC3_HDMI => 16#04_0000#,
       when USBC4_DP | USBC4_HDMI => 16#08_0000#,
       when USBC5_DP | USBC5_HDMI => 16#10_0000#,
       when USBC6_DP | USBC6_HDMI => 16#20_0000#,
       when others                => 0);

   function SHOTPLUG_CTL_DDI_HPD_ENABLE (Port : Combo_Port_Type) return Word32
   is (case Port is
       when DP1 | HDMI1 => 16#008#,
       when DP2 | HDMI2 => 16#080#,
       when DP3 | HDMI3 => 16#800#);

   function SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (Port : USBC_Port_Type)
   return Word32
   is (case Port is
       when USBC1_DP | USBC1_HDMI => 16#00_0008#,
       when USBC2_DP | USBC2_HDMI => 16#00_0080#,
       when USBC3_DP | USBC3_HDMI => 16#00_0800#,
       when USBC4_DP | USBC4_HDMI => 16#00_8000#,
       when USBC5_DP | USBC5_HDMI => 16#08_0000#,
       when USBC6_DP | USBC6_HDMI => 16#80_0000#);

   function TC_HOTPLUG_CTL_HPD_ENABLE (Port : USBC_Port_Type)
   return Word32
   is (SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (Port));

   procedure Initialize
   is
      INIT_DISPLAY_DETECTED : constant := 1 * 2 ** 0;
      Success : Boolean;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Registers.Unset_And_Set_Mask
        (Register => Registers.SHPD_FILTER_CNT,
         Mask_Unset => 16#1_ffff#,
         Mask_Set   => 16#1d9#);

      -- Hotplug for combo ports
      Registers.Set_Mask
        (Register => Registers.SHOTPLUG_CTL,
         Mask     =>
            SHOTPLUG_CTL_DDI_HPD_ENABLE (DP1) or -- also HDMI1
            SHOTPLUG_CTL_DDI_HPD_ENABLE (DP2) or -- also HDMI2
            SHOTPLUG_CTL_DDI_HPD_ENABLE (DP3));  -- also HDMI3

      -- Hotplug for Type-C ports in legacy mode
      Registers.Set_Mask
        (Register => Registers.SHOTPLUG_CTL_TC,
         Mask     =>
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC1_DP) or -- also USBC1_HDMI
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC2_DP) or -- also USBC2_HDMI
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC3_DP) or -- also USBC3_HDMI
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC4_DP) or -- also USBC4_HDMI
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC5_DP) or -- also USBC5_HDMI
            SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (USBC6_DP));  -- also USBC6_HDMI

      -- Hotplug for Type-C ports in DP-Alt mode
      Registers.Set_Mask
        (Register => Registers.TC_HOTPLUG_CTL,
         Mask     =>
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC1_DP) or -- also USBC1_HDMI
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC2_DP) or -- also USBC2_HDMI
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC3_DP) or -- also USBC3_HDMI
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC4_DP) or -- also USBC4_HDMI
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC5_DP) or -- also USBC5_HDMI
            TC_HOTPLUG_CTL_HPD_ENABLE (USBC6_DP));  -- also USBC6_HDMI

      -- Validity can only be detected via hotplug
      Config.Valid_Port (eDP) := True;
      for Port in DP1 .. HDMI3 loop
         Config.Valid_Port (Port) := True;
      end loop;

      -- For Type-C ports, we can check if the IOM PHY status is 'Complete',
      -- then perform the connection flow for all connected ports.
      Power_And_Clocks.Power_Up_Aux;
      for Port in USBC1_DP .. USBC6_HDMI loop
         declare
            G : GPU_Port;
            Success : Boolean;
         begin
            G := Config_Helpers.To_GPU_Port (Pipe_Index'First, Port);
            if G in USBC_Port then
               Connectors.TC.Connect (G, Success);
               Config.Valid_Port (Port) := Success;
            end if;
         end;
      end loop;

      -- TCCOLD should be blocked first before accessing FIA registers
      -- during e.g. connect flows. It can be unblocked only after
      -- done accessing FIA registers and there is no longer a
      -- connection i.e., after all ports are disconnected.
      -- In order to avoid keeping track of the state and constantly
      -- blocking and unblocking, we just block it once at the beginning and
      -- leave it that way.
      if not Config.Has_XELPD_Power_Domains then
         Connectors.TC.TC_Cold_Request (Connectors.TC.Block, Success);
         if not Success then
            Debug.Put_Line ("Failed to block TCCOLD, Type-C will not work!");
            for Port in USBC1_DP .. USBC6_HDMI loop
               Config.Valid_Port (Port) := False;
            end loop;
         end if;
      end if;
   end Initialize;

   procedure Hotplug_Detect
     (Port     : in Active_Port_Type;
      Detected : out Boolean)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      Detected := False;

      if Port = eDP then
         Detected := Config.Valid_Port (eDP);
         return;
      end if;

      -- DP-Alt HPD is in the north display hotplug registers
      if Port in USBC_Port_Type then
         Registers.Is_Set_Mask
           (Register => Registers.GEN11_DE_HPD_ISR,
            Mask     => DE_HPD_ISR_MASK (Port),
            Result   => Detected);
      end if;

      -- Legacy HDMI/DP hotplug is detected through the south display registers
      if Port in Combo_Port_Type then
         Registers.Is_Set_Mask
           (Register => Registers.SDEISR,
            Mask     => SDEISR_MASK (Port),
            Result   => Detected);
      end if;
   end Hotplug_Detect;

   procedure Clear_Hotplug_Detect (Port : Active_Port_Type)
   is
      Ignored_HPD : Boolean;
   begin
      pragma Warnings (GNATprove, Off, "unused assignment to ""Ignored_HPD""",
                       Reason => "We want to clear pending events only");
      Port_Detect.Hotplug_Detect (Port, Ignored_HPD);
      pragma Warnings (GNATprove, On, "unused assignment to ""Ignored_HPD""");
   end Clear_Hotplug_Detect;

end HW.GFX.GMA.Port_Detect;
