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

with HW.Debug;
with GNAT.Source_Info;
with HW.GFX.GMA.PLLs.Combo_Phy;
with HW.GFX.GMA.PLLs.DKL;
with HW.GFX.GMA.Config;

package body HW.GFX.GMA.PLLs
with
   Refined_State => (State => PLLs)
is

   type PLL_Type is (PLL_Unknown, PLL_Combo_Phy, PLL_DKL);
   function Port_PLL_Type (Port : GPU_Port) return PLL_Type is
     (case Port is
         when Combo_Port => PLL_Combo_Phy,
         when USBC_Port => PLL_DKL,
         when others => PLL_Unknown);

   type Count_Range is new Natural range 0 .. 2;
   type PLL_State is record
      Use_Count  : Count_Range;
      Used_For_DP : Boolean;
      Link_Rate   : DP_Bandwidth;
      Mode        : Mode_Type;
   end record;

   type PLL_State_Array is array (Configurable_DPLLs) of PLL_State;

   pragma Warnings (Off, "unused variable ""PLLs""",
                    Reason => "Not yet implemented.");
   pragma Warnings (Off, "variable ""PLLs"" is assigned but never read",
                    Reason => "Not yet implemented.");
   PLLs : PLL_State_Array;
   pragma Warnings (On, "variable ""PLLs"" is assigned but never read");
   pragma Warnings (On, "unused variable ""PLLs""");

   procedure Initialize is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      PLLs :=
        (Configurable_DPLLs =>
           (Use_Count   => 0,
            Used_For_DP => False,
            Link_Rate   => DP_Bandwidth'First,
            Mode        => Invalid_Mode));
   end Initialize;

   procedure Alloc
     (Port_Cfg : in     Port_Config;
      PLL      :    out T;
      Success  :    out Boolean)
   is
      function Port_PLL_Match (Port : GPU_Port; PLL : T) return Boolean is
        (case Port is
            when Combo_Port => PLL in Combo_DPLLs,
            when DDI_TC1    => PLL = TCPLL1,
            when DDI_TC2    => PLL = TCPLL2,
            when DDI_TC3    => PLL = TCPLL3,
            when DDI_TC4    => PLL = TCPLL4,
            when DDI_TC5    => PLL = TCPLL5,
            when DDI_TC6    => PLL = TCPLL6,
            when others     => False);

      function Config_Matches (PE : HW.GFX.GMA.PLLs.PLL_State) return Boolean
      is
      begin
         return
            PE.Used_For_DP = (Port_Cfg.Display = DP) and
            ((PE.Used_For_DP and PE.Link_Rate = Port_Cfg.DP.Bandwidth) or
             (not PE.Used_For_DP and PE.Mode = Port_Cfg.Mode));
      end Config_Matches;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      for P in Configurable_DPLLs loop
         Success := PLLs (P).Use_Count /= 0 and
                     PLLs (P).Use_Count /= Count_Range'Last and
                     Port_PLL_Match (Port_Cfg.Port, P) and
                     Config_Matches (PLLs (P));
         if Success then
            PLL := P;
            PLLs (PLL).Use_Count := PLLs (PLL).Use_Count + 1;
            return;
         end if;
      end loop;

      for P in Configurable_DPLLs loop
         if PLLs (P).Use_Count = 0 and Port_PLL_Match (Port_Cfg.Port, P) then
            PLL := P;
            case P is
               when Combo_DPLLs => Combo_Phy.On (PLL, Port_Cfg, Success);
               when DKL_DPLLs   => DKL.On (PLL, Port_Cfg, Success);
            end case;
            if Success then
               PLLs (PLL) :=
                 (Use_Count   => 1,
                  Used_For_DP => Port_Cfg.Display = DP,
                  Link_Rate   => Port_Cfg.DP.Bandwidth,
                  Mode        => Port_Cfg.Mode);
            end if;
            return;
         end if;
      end loop;

      PLL := Invalid;
      Success := False;
   end Alloc;

   procedure Free (PLL : T) is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      if PLL /= Invalid_PLL then
         PLLs (PLL).Use_Count := 0;
      end if;

      case PLL is
         when Combo_DPLLs => Combo_Phy.Free (PLL);
         when DKL_DPLLs   => DKL.Free (PLL);
         when Invalid_PLL => null;
      end case;
   end Free;

   procedure All_Off is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));
      Combo_Phy.All_Off;
      DKL.All_Off;
   end All_Off;

   function Register_Value (PLL : T) return Word32
   is
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      -- Only required for combo ports
      if PLL in Combo_DPLLs then
         return Combo_Phy.Register_Value (PLL);
      end if;

      return 0;
   end Register_Value;
end HW.GFX.GMA.PLLs;
