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

private package HW.GFX.GMA.GMCH is

   GMCH_PORT_PIPE_SELECT_SHIFT : constant := 30;
   GMCH_PORT_PIPE_SELECT_MASK  : constant := 1 * 2 ** 30;
   type GMCH_PORT_PIPE_SELECT_Array is array (Pipe_Index) of Word32;
   GMCH_PORT_PIPE_SELECT       : constant GMCH_PORT_PIPE_SELECT_Array :=
     (Primary   => 0 * 2 ** GMCH_PORT_PIPE_SELECT_SHIFT,
      Secondary => 1 * 2 ** GMCH_PORT_PIPE_SELECT_SHIFT,
      Tertiary  => 0);

end HW.GFX.GMA.GMCH;
