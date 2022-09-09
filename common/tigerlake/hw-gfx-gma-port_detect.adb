--
-- Copyright (C) Google, LLC
-- Copyright (C) 2024 secunet Security Networks AG
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

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Port_Detect
is
   SHOTPLUG_CTL_DDI_HPD_STATUS_MASK    : constant := 16#0333#;

   function SHOTPLUG_CTL_DDI_HPD_ENABLE (P : Combo_Port) return Word32 is
     (Shift_Left (8, 4 * (GPU_Port'Pos (P) - GPU_Port'Pos (Combo_Port'First))));

   function SHOTPLUG_CTL_DDI_HPD_STATUS (P : Combo_Port) return Word32 is
     (Shift_Left (3, 4 * (GPU_Port'Pos (P) - GPU_Port'Pos (Combo_Port'First))));

   function SHOTPLUG_CTL_DDI_HPD_LONG_DETECT (P : Combo_Port) return Word32 is
     (Shift_Left (2, 4 * (GPU_Port'Pos (P) - GPU_Port'Pos (Combo_Port'First))));

   SHOTPLUG_CTL_TC_DDI_HPD_STATUS_MASK : constant := 16#0033_3333#;

   function SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (P : USBC_Port) return Word32 is
     (Shift_Left (8, 4 * (GPU_Port'Pos (P) - GPU_Port'Pos (USBC_Port'First))));

   function SHOTPLUG_CTL_TC_DDI_HPD_STATUS (P : USBC_Port) return Word32 is
     (Shift_Left (3, 4 * (GPU_Port'Pos (P) - GPU_Port'Pos (USBC_Port'First))));

   function SHOTPLUG_CTL_TC_DDI_HPD_LONG_DETECT (P : USBC_Port) return Word32 is
     (Shift_Left (2, 4 * (GPU_Port'Pos (P) - GPU_Port'Pos (USBC_Port'First))));

   TC_HOTPLUG_CTL_HPD_STATUS_MASK : constant := 16#0033_3333#;

   function TC_HOTPLUG_CTL_HPD_ENABLE (P : USBC_Port) return Word32 renames
      SHOTPLUG_CTL_TC_DDI_HPD_ENABLE;

   function TC_HOTPLUG_CTL_HPD_STATUS (P : USBC_Port) return Word32 renames
      SHOTPLUG_CTL_TC_DDI_HPD_STATUS;

   procedure Initialize
   is
      Success : Boolean;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Registers.Unset_And_Set_Mask
        (Register => Registers.SHPD_FILTER_CNT,
         Mask_Unset => 16#1_ffff#,
         Mask_Set   => 16#1d9#); -- constant from i915 (SHPD_FILTER_CNT_500_ADJ)

      -- Hotplug for combo ports
      Registers.Set_Mask
        (Register => Registers.SHOTPLUG_CTL,
         Mask     =>
            SHOTPLUG_CTL_DDI_HPD_ENABLE (DIGI_A) or
            SHOTPLUG_CTL_DDI_HPD_ENABLE (DIGI_B) or
            SHOTPLUG_CTL_DDI_HPD_ENABLE (DIGI_C) or
            SHOTPLUG_CTL_DDI_HPD_STATUS_MASK); -- clear status

      -- Validity can only be detected via hotplug.
      -- DP3/HDMI3 doesn't exist in any TGL SKU.
      for Port in DP1 .. DP2 loop
         Config.Valid_Port (Port) := True;
      end loop;
      for Port in DP_TC1 .. DP_TC4 loop
         Config.Valid_Port (Port) := True;
      end loop;
      for Port in HDMI1 .. HDMI2 loop
         Config.Valid_Port (Port) := True;
      end loop;
      for Port in HDMI_TC1 .. HDMI_TC4 loop
         Config.Valid_Port (Port) := True;
      end loop;
      for Port in USBC1 .. USBC4 loop
         Config.Valid_Port (Port) := True;
      end loop;

      for Port in DDI_TC1 .. Config.Last_TC_Port loop
         -- Hotplug for Type-C ports in legacy mode
         Registers.Set_Mask
           (Register => Registers.SHOTPLUG_CTL_TC,
            Mask     => SHOTPLUG_CTL_TC_DDI_HPD_ENABLE (Port));
         -- Hotplug for Type-C ports in DP-Alt mode
         Registers.Set_Mask
           (Register => Registers.TC_HOTPLUG_CTL,
            Mask     => TC_HOTPLUG_CTL_HPD_ENABLE (Port));
      end loop;

      -- Clear status:
      Registers.Set_Mask
        (Register => Registers.SHOTPLUG_CTL_TC,
         Mask     => SHOTPLUG_CTL_TC_DDI_HPD_STATUS_MASK);
      Registers.Set_Mask
        (Register => Registers.TC_HOTPLUG_CTL,
         Mask     => TC_HOTPLUG_CTL_HPD_STATUS_MASK);

      -- TCCOLD should be blocked first before accessing FIA registers
      -- during e.g. connect flows. It can be unblocked only after done
      -- accessing FIA registers and there is no longer a connection
      -- i.e., after all ports are disconnected.
      -- In order to avoid keeping track of the state and constantly
      -- blocking and unblocking, we just block it once at the beginning
      -- and leave it that way.
      Connectors.TC.TC_Cold_Request (Connectors.TC.Block, Success);
      if not Success then
         Debug.Put_Line ("Failed to block TCCOLD, Type-C will not work!");
         return;
      end if;
   end Initialize;

   procedure Hotplug_Detect
     (Port     : in Active_Port_Type;
      Detected : out Boolean)
   is
      GPU_Port : constant GMA.GPU_Port := Config_Helpers.To_GPU_Port (Pipe_Index'First, Port);
      North32, South32 : Word32;
   begin
      Detected := False;

      if GPU_Port in USBC_Port then
         Registers.Read (Registers.TC_HOTPLUG_CTL, North32, Verbose => False);
         Registers.Read (Registers.SHOTPLUG_CTL_TC, South32, Verbose => False);

         Detected := ((North32 or South32) and
            SHOTPLUG_CTL_TC_DDI_HPD_LONG_DETECT (GPU_Port)) /= 0;

         if (North32 and TC_HOTPLUG_CTL_HPD_STATUS (GPU_Port)) /= 0 then
            Registers.Unset_And_Set_Mask
              (Register    => Registers.TC_HOTPLUG_CTL,
               Mask_Unset  => TC_HOTPLUG_CTL_HPD_STATUS_MASK,
               Mask_Set    => TC_HOTPLUG_CTL_HPD_STATUS (GPU_Port));
         end if;
         if (South32 and TC_HOTPLUG_CTL_HPD_STATUS (GPU_Port)) /= 0 then
            Registers.Unset_And_Set_Mask
              (Register    => Registers.SHOTPLUG_CTL_TC,
               Mask_Unset  => SHOTPLUG_CTL_TC_DDI_HPD_STATUS_MASK,
               Mask_Set    => SHOTPLUG_CTL_TC_DDI_HPD_STATUS (GPU_Port));
         end if;
      end if;

      if GPU_Port in Combo_Port then
         Registers.Read (Registers.SHOTPLUG_CTL, South32, Verbose => False);
         Detected := (South32 and SHOTPLUG_CTL_DDI_HPD_LONG_DETECT (GPU_Port)) /= 0;

         if (South32 and SHOTPLUG_CTL_DDI_HPD_STATUS (GPU_Port)) /= 0 then
            Registers.Unset_And_Set_Mask
              (Register    => Registers.SHOTPLUG_CTL,
               Mask_Unset  => SHOTPLUG_CTL_DDI_HPD_STATUS_MASK,
               Mask_Set    => SHOTPLUG_CTL_DDI_HPD_STATUS (GPU_Port));
         end if;
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
