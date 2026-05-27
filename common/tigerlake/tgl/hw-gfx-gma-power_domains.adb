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
with HW.GFX.GMA.Config_Helpers;
with HW.GFX.GMA.Connectors.TC.Ownership;
with HW.GFX.GMA.Power_Domains_Common;
use HW.GFX.GMA.Power_Domains_Common;

package body HW.GFX.GMA.Power_Domains is

   function Need_PW (PW : Dynamic_Well; Configs : Pipe_Configs) return Boolean
   is
      function Any_TC_Port return Boolean is
        (for some Pipe in Pipe_Index =>
            Configs (Pipe).Port /= Disabled and then
            Config_Helpers.To_GPU_Port (Pipe, Configs (Pipe).Port) in USBC_Port);

      function Any_Pipe_From (First : Pipe_Index) return Boolean is
        (for some Pipe in First .. Pipe_Index'Last => Configs (Pipe).Port /= Disabled);

      function VGA return Boolean is
        (Configs (Primary).Framebuffer.Offset = VGA_PLANE_FRAMEBUFFER_OFFSET);
   begin
      case PW is
         when PW2 | PW3 =>
            return Any_Pipe_From (Secondary) or Any_TC_Port or VGA;
         when PW4 =>
            return Any_Pipe_From (Tertiary);
         when PW5 =>
            return False;  -- Fourth pipe not supported yet.
      end case;
   end Need_PW;

   ----------------------------------------------------------------------------

   procedure Power_Up (Port : Active_Port_Type; Success : out Boolean)
   is
      GPU_Port : constant GMA.GPU_Port :=
         Config_Helpers.To_GPU_Port (Pipe_Index'First, Port);

      procedure On (Aux : AUX_Domain; DDI : DDI_Domain) is
      begin
         PD_On (PW1);
         if GPU_Port in USBC_Port then
            PD_On (PW2);
            PD_On (PW3);
         end if;
         PD_On (DDI);

         if GPU_Port in USBC_Port then
            Connectors.TC.Ownership.Claim
              (Port     => GPU_Port,
               DP_Alt   => Port in Physical_USBC_Ports,
               Success  => Success);
            if not Success then
               return;
            end if;
         end if;
         PD_On (Aux);

         Success := True;
      end On;
   begin
      pragma Debug (Debug.Put_Line (GNAT.Source_Info.Enclosing_Entity));

      case GPU_Port is
         when GMA.DIGI_A   => On (AUX_A, DDI_A);
         when GMA.DIGI_B   => On (AUX_B, DDI_B);
         when GMA.DIGI_C   => On (AUX_C, DDI_C);
         when GMA.DDI_TC1  => On (AUX_USBC1, DDI_USBC1);
         when GMA.DDI_TC2  => On (AUX_USBC2, DDI_USBC2);
         when GMA.DDI_TC3  => On (AUX_USBC3, DDI_USBC3);
         when GMA.DDI_TC4  => On (AUX_USBC4, DDI_USBC4);
         when GMA.DDI_TC5  => On (AUX_USBC5, DDI_USBC5);
         when GMA.DDI_TC6  => On (AUX_USBC6, DDI_USBC6);
         when others       => Success := True;
      end case;
   end Power_Up;

end HW.GFX.GMA.Power_Domains;
