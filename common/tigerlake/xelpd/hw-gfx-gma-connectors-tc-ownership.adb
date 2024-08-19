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

with HW.GFX.GMA.Config;
with HW.GFX.GMA.Registers;
with HW.GFX.DP_Info;
with HW.GFX.GMA.PCode;

with HW.Debug;
with GNAT.Source_Info;

use type HW.Word64;

package body HW.GFX.GMA.Connectors.TC.Ownership is

   type Port_Regs_Array is array (Valid_TC_Port) of Registers.Registers_Index;

   TCSS_DDI_STATUS : constant Port_Regs_Array :=
      Port_Regs_Array'
     (DDI_TC1 => Registers.TCSS_DDI_STATUS_1,
      DDI_TC2 => Registers.TCSS_DDI_STATUS_2,
      DDI_TC3 => Registers.TCSS_DDI_STATUS_3,
      DDI_TC4 => Registers.TCSS_DDI_STATUS_4);

   TCSS_DDI_STATUS_HPD_LIVE_STATUS_ALT : constant := 1 * 2 ** 0;
   TCSS_DDI_STATUS_READY               : constant := 1 * 2 ** 2;

   procedure Claim
     (Port     : in     USBC_Port;
      DP_Alt   : in     Boolean;
      Success  :    out Boolean)
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Port not in Valid_TC_Port then
         Success := False;
         return;
      end if;

      if not DP_Alt then
         -- For legacy ports, this is supposed
         -- to be initialized once during boot.
         Registers.Wait_Set_Mask
           (Register => TCSS_DDI_STATUS (Port),
            Mask     => TCSS_DDI_STATUS_READY,
            TOut_MS  => 100);
      end if;

      Registers.Is_Set_Mask
        (Register => TCSS_DDI_STATUS (Port),
         Mask     => TCSS_DDI_STATUS_READY,
         Result   => Success);
      if not Success then
         pragma Debug (Debug.Put_Line ("DP PHY mode status not complete"));
         return;
      end if;

      Registers.Set_Mask
        (Register => DDI_BUF_CTL (Port),
         Mask     => DDI_BUF_CTL_TC_PHY_OWNERSHIP);
   end Claim;

   procedure Claimed (Port : USBC_Port; Is_Claimed : out Boolean)
   with
      Refined_Post => (if Is_Claimed then Port in Valid_TC_Port)
   is
   begin
      if Port not in Valid_TC_Port then
         Is_Claimed := False;
         return;
      end if;

      Registers.Is_Set_Mask
        (Register => DDI_BUF_CTL (Port),
         Mask     => DDI_BUF_CTL_TC_PHY_OWNERSHIP,
         Result   => Is_Claimed);
   end Claimed;

   ---------------------------------------------------------------------

   procedure Connect
     (Port     : in     USBC_Port;
      DP_Alt   : in     Boolean;
      Lanes    : in     DP_Lane_Count;
      Success  :    out Boolean)
   is
      Assigned_Lanes : DP_Lane_Count;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      Claimed (Port, Success);
      if not Success then
         pragma Debug (Debug.Put_Line ("Tried to connect to unclaimed port."));
         return;
      end if;

      if DP_Alt then
         Registers.Is_Set_Mask
           (Register => TCSS_DDI_STATUS (Port),
            Mask     => TCSS_DDI_STATUS_HPD_LIVE_STATUS_ALT,
            Result   => Success);
         if not Success then
            pragma Debug (Debug.Put_Line ("DP-Alt is not connected."));
            return;
         end if;

         Get_Lane_Assignment_Count (Port, Assigned_Lanes);
         Set_Lane_Count (Port, Assigned_Lanes);
      else
         Set_Lane_Count (Port, Lanes);
      end if;
   end Connect;

   procedure Disconnect (Port : USBC_Port) is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if Port in Valid_TC_Port then
         Registers.Unset_Mask
           (Register => DDI_BUF_CTL (Port),
            Mask     => DDI_BUF_CTL_TC_PHY_OWNERSHIP);
      end if;
   end Disconnect;

end HW.GFX.GMA.Connectors.TC.Ownership;
