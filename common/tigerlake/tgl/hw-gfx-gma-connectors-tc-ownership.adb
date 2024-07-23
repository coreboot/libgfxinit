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

with HW.Debug;
with GNAT.Source_Info;

package body HW.GFX.GMA.Connectors.TC.Ownership is

   procedure Claim
     (Port     : in     USBC_Port;
      DP_Alt   : in     Boolean;
      Success  :    out Boolean)
   is
   begin
      -- For legacy ports, this is supposed to be
      -- initialized once during boot, hence wait.
      Registers.Wait_Set_Mask
        (Register => Fia_Regs (Port).PORT_TX_DFLEXDPPMS,
         Mask     => DP_PHY_MODE_STATUS_COMPLETE (Port),
         TOut_MS  => (if DP_Alt then 0 else 100),
         Success  => Success);
      if not Success then
         pragma Debug (Debug.Put_Line ("DP PHY mode status not complete"));
         return;
      end if;

      Registers.Set_Mask
        (Register => Fia_Regs (Port).PORT_TX_DFLEXDPCSSS,
         Mask     => DP_PHY_MODE_STATUS_NOT_SAFE (Port));
   end Claim;

   procedure Claimed (Port : USBC_Port; Is_Claimed : out Boolean) is
   begin
      if Port not in Valid_TC_Port then
         Is_Claimed := False;
         return;
      end if;

      Registers.Is_Set_Mask
        (Register => Fia_Regs (Port).PORT_TX_DFLEXDPCSSS,
         Mask     => DP_PHY_MODE_STATUS_NOT_SAFE (Port),
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
      Claimed (Port, Success);
      if not Success then
         pragma Debug (Debug.Put_Line ("Tried to connect to unclaimed port."));
         return;
      end if;

      if DP_Alt then
         Registers.Is_Set_Mask
           (Register => Fia_Regs (Port).PORT_TX_DFLEXDPSP,
            Mask     => TC_LIVE_STATE_TC (Port),
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
      if Port in Valid_TC_Port then
         Registers.Unset_Mask
           (Register => Fia_Regs (Port).PORT_TX_DFLEXDPCSSS,
            Mask     => DP_PHY_MODE_STATUS_NOT_SAFE (Port));
      end if;
   end Disconnect;

end HW.GFX.GMA.Connectors.TC.Ownership;
