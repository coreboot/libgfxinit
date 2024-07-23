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
      Success := False;
   end Claim;

   procedure Claimed (Port : USBC_Port; Is_Claimed : out Boolean) is
   begin
      Is_Claimed := False;
   end Claimed;

   ---------------------------------------------------------------------

   procedure Connect
     (Port     : in     USBC_Port;
      DP_Alt   : in     Boolean;
      Lanes    : in     DP_Lane_Count;
      Success  :    out Boolean)
   is
   begin
      Success := False;
   end Connect;

   procedure Disconnect (Port : USBC_Port) is null;

end HW.GFX.GMA.Connectors.TC.Ownership;
