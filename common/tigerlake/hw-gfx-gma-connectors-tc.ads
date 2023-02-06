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

with HW.GFX.GMA.DP_Info;

package HW.GFX.GMA.Connectors.TC is

   procedure Claim
     (Port     : in     USBC_Port;
      DP_Alt   : in     Boolean;
      Success  :    out Boolean);
   procedure Claimed (Port : USBC_Port; Is_Claimed : out Boolean);
   procedure Connect
     (Port     : in     USBC_Port;
      DP_Alt   : in     Boolean;
      Lanes    : in     DP_Lane_Count;
      Success  :    out Boolean);
   procedure Disconnect (Port : USBC_Port);

   procedure Program_DP_Mode (P : USBC_Port; Lane_Count : Natural);
   procedure Enable_HDMI (Port : USBC_Port);

   procedure Set_Signal_Levels
     (Port        : USBC_Port;
      Link        : DP_Link;
      Train_Set   : DP_Info.Train_Set);

   type TC_Cold_Request_Type is (Block, Unblock);
   procedure TC_Cold_Request
     (Request : in     TC_Cold_Request_Type;
      Success :    out Boolean);

end HW.GFX.GMA.Connectors.TC;
