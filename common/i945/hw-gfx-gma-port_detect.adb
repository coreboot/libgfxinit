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

with HW.GFX.GMA.Config;
with HW.GFX.GMA.Registers;

package body HW.GFX.GMA.Port_Detect
is

   CRT_HOTPLUG_INT_EN               : constant := 1 * 2 ** 9;
   CRT_HOTPLUG_ACTIVATION_PERIOD_64 : constant := 1 * 2 ** 8;

   HOTPLUG_INT_STATUS : constant array (Active_Port_Type) of Word32 :=
     (Analog => 1 * 2 ** 11,
      others => 0);

   procedure Initialize
   is
   begin
      -- i945: VGA (ADPA) is always present
      Config.Valid_Port (Analog) := True;

      -- LVDS is only present on mobile Gen3 parts (i945GM/Pineview-M)
      Config.Valid_Port (LVDS)   := Config.GMCH_Gen3_Mobile;

      -- Enable CRT hotplug detection
      Registers.Write
        (Register => Registers.PORT_HOTPLUG_EN,
         Value    => CRT_HOTPLUG_INT_EN or CRT_HOTPLUG_ACTIVATION_PERIOD_64);
   end Initialize;

   procedure Hotplug_Detect (Port : in Active_Port_Type; Detected : out Boolean)
   is
      Ctl32 : Word32;
   begin
      Registers.Read (Register => Registers.PORT_HOTPLUG_STAT,
                      Value    => Ctl32);
      Detected := (Ctl32 and HOTPLUG_INT_STATUS (Port)) /= 0;

      if Detected then
         Registers.Set_Mask
           (Register => Registers.PORT_HOTPLUG_STAT,
            Mask     => HOTPLUG_INT_STATUS (Port));
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
