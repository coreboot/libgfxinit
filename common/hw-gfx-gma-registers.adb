--
-- Copyright (C) 2015-2016 secunet Security Networks AG
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

with HW.Time;
with HW.MMIO_Range;
pragma Elaborate_All (HW.MMIO_Range);

with HW.Debug;
with GNAT.Source_Info;

use type HW.Word64;

package body HW.GFX.GMA.Registers
with
   Refined_State =>
     (Address_State  =>
        (Regs.Base_Address, GTT_32.Base_Address, GTT_64.Base_Address),
      Register_State => Regs.State,
      GTT_State      => (GTT_32.State, GTT_64.State))
is
   pragma Disable_Atomic_Synchronization;

   type Registers_Range is
      new Natural range 0 .. 16#0020_0000# / Register_Width - 1;
   type Registers_Type is array (Registers_Range) of Word32
   with
      Atomic_Components,
      Size => 16#20_0000# * 8;
   package Regs is new MMIO_Range
     (Base_Addr   => Config.Default_MMIO_Base,
      Element_T   => Word32,
      Index_T     => Registers_Range,
      Array_T     => Registers_Type);

   ----------------------------------------------------------------------------

   MMIO_GTT_32_Size : constant := 16#20_0000#;
   -- Limit Broadwell+ to 4MiB to have a stable
   -- interface (i.e. same number of entries):
   MMIO_GTT_64_Size : constant := 16#40_0000#;

   type GTT_PTE_32 is mod 2 ** 32;
   type GTT_Registers_32 is array (GTT_Range) of GTT_PTE_32
   with
      Volatile_Components,
      Size => MMIO_GTT_32_Size * 8;
   package GTT_32 is new MMIO_Range
     (Base_Addr   =>
        Config.Default_MMIO_Base + Word64 (Config.Default_MMIO_GTT_32_Offset),
      Element_T   => GTT_PTE_32,
      Index_T     => GTT_Range,
      Array_T     => GTT_Registers_32);

   type GTT_PTE_64 is mod 2 ** 64;
   type GTT_Registers_64 is array (GTT_Range) of GTT_PTE_64
   with
      Volatile_Components,
      Size => MMIO_GTT_64_Size * 8;
   package GTT_64 is new MMIO_Range
     (Base_Addr   => Config.Default_MMIO_Base + Word64 (Config.MMIO_GTT_64_Offset),
      Element_T   => GTT_PTE_64,
      Index_T     => GTT_Range,
      Array_T     => GTT_Registers_64);

   GTT_PTE_Valid : constant := 1;

   ----------------------------------------------------------------------------

   subtype Fence_Range is Natural range 0 .. 31;

   FENCE_PAGE_SHIFT                    : constant := 12;
   FENCE_PAGE_MASK                     : constant := 16#ffff_f000#;
   FENCE_VALID                         : constant := 1 * 2 ** 0;

   -- Gen4+ (i965/G45+): 64-bit fence pairs at Fence_Base + i*8
   FENCE_TILE_WALK_YMAJOR              : constant := 1 * 2 ** 1;

   function Fence_Lower_Idx (Fence : Fence_Range) return Registers_Range is
      (Registers_Range (Config.Fence_Base / Register_Width + 2 * Fence));
   function Fence_Upper_Idx (Fence : Fence_Range) return Registers_Range is
      (Fence_Lower_Idx (Fence) + 1);

   -- Gen3 (i915/i945): 32-bit fences, split layout:
   --   Fences 0-7:  0x2000 + i*4
   --   Fences 8-15: 0x3000 + (i-8)*4
   -- FENCE_REG(i) = 0x2000 + (((i) & 8) << 9) + ((i) & 7) * 4
   GEN3_FENCE_TILING_Y_SHIFT           : constant := 12;
   GEN3_FENCE_SIZE_SHIFT               : constant := 8;
   GEN3_FENCE_PITCH_SHIFT              : constant := 4;

   function Gen3_Fence_Idx (Fence : Fence_Range) return Registers_Range is
      (Registers_Range
         ((16#2000# + (Fence / 8) * 16#1000# + (Fence mod 8) * 4) /
          Register_Width));

   -- Compute floor(log2(n)) for n >= 1 (fence size/pitch encoding).
   function Floor_Log2 (N : Word32) return Natural
   with
      Pre => N >= 1
   is
      Result : Natural := 0;
      Val    : Word32 := N;
   begin
      for I in 0 .. 31 loop
         exit when Val <= 1;
         Val    := Shift_Right (Val, 1);
         Result := I + 1;
      end loop;
      return Result;
   end Floor_Log2;

   procedure Clear_Fences
   is
   begin
      if Config.Has_Gen3_Fences then
         for Fence in Fence_Range range 0 .. Config.Fence_Count - 1 loop
            Regs.Write (Gen3_Fence_Idx (Fence), 0);
         end loop;
      else
         for Fence in Fence_Range range 0 .. Config.Fence_Count - 1 loop
            Regs.Write (Fence_Lower_Idx (Fence), 0);
         end loop;
      end if;
   end Clear_Fences;

   procedure Add_Fence
     (First_Page  : in     GTT_Range;
      Last_Page   : in     GTT_Range;
      Tiling      : in     XY_Tiling;
      Pitch       : in     Natural;
      Success     :    out Boolean)
   is
      Y_Tiles : constant Boolean := Tiling = Y_Tiled;
      Reg32 : Word32;
   begin
      pragma Debug (Debug.Put (GNAT.Source_Info.Enclosing_Entity & ": "));
      pragma Debug (Debug.Put_Word32 (Shift_Left (Word32 (First_Page), 12)));
      pragma Debug (Debug.Put (":"));
      pragma Debug (Debug.Put_Word32 (Shift_Left (Word32 (Last_Page), 12)));
      pragma Debug (not Y_Tiles, Debug.Put (" X tiled in "));
      pragma Debug (    Y_Tiles, Debug.Put (" Y tiled in "));
      pragma Debug (Debug.Put_Int32 (Int32 (Pitch)));
      pragma Debug (Debug.Put_Line (" tiles per row."));

      Success := False;

      if Config.Has_Gen3_Fences then
         -- Gen3 i945: single 32-bit fence register per fence
         -- Format: start[31:20] | tiling_y[12] | size_bits[11:8] |
         --         pitch_log2[7:4] | valid[0]
         -- stride: Y-tiled /128, X-tiled /512 (i945 has 128-byte Y tiling)
         -- size_bits: log2(size_in_pages / 256) = log2(size_in_MB)
         for Fence in Fence_Range range 0 .. Config.Fence_Count - 1 loop
            Regs.Read (Reg32, Gen3_Fence_Idx (Fence));
            if (Reg32 and FENCE_VALID) = 0 then
               declare
                  Start_Addr : constant Word32 :=
                     Shift_Left (Word32 (First_Page), FENCE_PAGE_SHIFT);
                  Size_Pages : constant Word32 :=
                     Word32 (Last_Page - First_Page + 1);
                  -- Size in MB (pages / 256, since page = 4KB, 256*4KB = 1MB)
                  Size_MB    : constant Word32 := Size_Pages / 256;
                  -- Pitch in tiles (X: 512B tiles, Y: 128B tiles for i945)
                  Stride     : constant Word32 :=
                     Word32 (Pitch) / (if Y_Tiles then 128 else 512);
                  Size_Bits  : constant Word32 :=
                     (if Size_MB >= 1
                      then Word32 (Floor_Log2 (Size_MB))
                      else 0);
               begin
                  Regs.Write
                    (Index => Gen3_Fence_Idx (Fence),
                     Value => Start_Addr or
                              (if Y_Tiles
                               then Shift_Left (1, GEN3_FENCE_TILING_Y_SHIFT)
                               else 0) or
                              Shift_Left (Size_Bits, GEN3_FENCE_SIZE_SHIFT) or
                              Shift_Left
                                ((if Stride >= 1
                                  then Word32 (Floor_Log2 (Stride))
                                  else 0),
                                 GEN3_FENCE_PITCH_SHIFT) or
                              FENCE_VALID);
               end;
               Success := True;
               exit;
            end if;
         end loop;
      else
         -- Gen4+ (i965/G45+): 64-bit fence register pairs
         for Fence in Fence_Range range 0 .. Config.Fence_Count - 1 loop
            Regs.Read (Reg32, Fence_Lower_Idx (Fence));
            if (Reg32 and FENCE_VALID) = 0 then
               Regs.Write
                 (Index => Fence_Lower_Idx (Fence),
                  Value => Shift_Left (Word32 (First_Page), FENCE_PAGE_SHIFT) or
                           (if Y_Tiles then FENCE_TILE_WALK_YMAJOR else 0) or
                           FENCE_VALID);
               Regs.Write
                 (Index => Fence_Upper_Idx (Fence),
                  Value => Shift_Left (Word32 (Last_Page), FENCE_PAGE_SHIFT) or
                           Word32 (Pitch) * (if Y_Tiles then 1 else 4) - 1);
               Success := True;
               exit;
            end if;
         end loop;
      end if;
   end Add_Fence;

   procedure Remove_Fence (First_Page, Last_Page : GTT_Range)
   is
      Page_Lower : constant Word32 :=
         Shift_Left (Word32 (First_Page), FENCE_PAGE_SHIFT);
      Reg32 : Word32;
   begin
      if Config.Has_Gen3_Fences then
         -- Gen3: match start address in single 32-bit register
         for Fence in Fence_Range range 0 .. Config.Fence_Count - 1 loop
            Regs.Read (Reg32, Gen3_Fence_Idx (Fence));
            if (Reg32 and FENCE_VALID) /= 0 and
               (Reg32 and 16#fff0_0000#) = (Page_Lower and 16#fff0_0000#)
            then
               Regs.Write (Gen3_Fence_Idx (Fence), 0);
               exit;
            end if;
         end loop;
      else
         -- Gen4+: match start in lower, end in upper register
         declare
            Page_Upper : constant Word32 :=
               Shift_Left (Word32 (Last_Page), FENCE_PAGE_SHIFT);
            Fence_Upper, Fence_Lower : Word32;
         begin
            for Fence in Fence_Range range 0 .. Config.Fence_Count - 1 loop
               Regs.Read (Fence_Lower, Fence_Lower_Idx (Fence));
               Regs.Read (Fence_Upper, Fence_Upper_Idx (Fence));
               if (Fence_Lower and FENCE_PAGE_MASK) = Page_Lower and
                  (Fence_Upper and FENCE_PAGE_MASK) = Page_Upper
               then
                  Regs.Write (Fence_Lower_Idx (Fence), 0);
                  exit;
               end if;
            end loop;
         end;
      end if;
   end Remove_Fence;

   ----------------------------------------------------------------------------

   procedure Write_GTT
     (GTT_Page       : GTT_Range;
      Device_Address : GTT_Address_Type;
      Valid          : Boolean)
   is
   begin
      if Config.Has_I945_Simple_GTT_PTE then
         -- i945: simple 32-bit PTE, no high address bits
         GTT_32.Write
           (Index => GTT_Page,
            Value => GTT_PTE_32 (Device_Address and 16#ffff_f000#) or
                     Boolean'Pos (Valid));
      elsif not Config.Has_64bit_GTT then
         GTT_32.Write
           (Index => GTT_Page,
            Value => GTT_PTE_32 (Device_Address and 16#ffff_f000#) or
                     GTT_PTE_32 (Shift_Right (Word64 (Device_Address), 32 - 4)
                                   and 16#0000_07f0#) or
                     Boolean'Pos (Valid));
      else
         GTT_64.Write
           (Index => GTT_Page,
            Value => GTT_PTE_64 (Device_Address and 16#7f_ffff_f000#) or
                     Boolean'Pos (Valid));
      end if;
   end Write_GTT;

   procedure Read_GTT
     (Device_Address :    out GTT_Address_Type;
      Valid          :    out Boolean;
      GTT_Page       : in     GTT_Range)
   is
   begin
      if Config.Has_I945_Simple_GTT_PTE then
         declare
            PTE : GTT_PTE_32;
         begin
            GTT_32.Read (PTE, GTT_Page);
            Valid := (PTE and GTT_PTE_Valid) /= 0;
            Device_Address := GTT_Address_Type (PTE and 16#ffff_f000#);
         end;
      elsif not Config.Has_64bit_GTT then
         declare
            PTE : GTT_PTE_32;
         begin
            GTT_32.Read (PTE, GTT_Page);
            Valid := (PTE and GTT_PTE_Valid) /= 0;
            Device_Address := GTT_Address_Type
              (Shift_Left (Word64 (PTE and 16#07f0#), 32 - 4) or
               Word64 (PTE and 16#ffff_f000#));
         end;
      else
         declare
            PTE : GTT_PTE_64;
         begin
            GTT_64.Read (PTE, GTT_Page);
            Valid := (PTE and GTT_PTE_Valid) /= 0;
            Device_Address := GTT_Address_Type (PTE and 16#7f_ffff_f000#);
         end;
      end if;
   end Read_GTT;

   ----------------------------------------------------------------------------

   package Rep is
      function Index (Reg : Registers_Index) return Registers_Range;
   end Rep;

   package body Rep is
      function Index (Reg : Registers_Index) return Registers_Range
      with
         SPARK_Mode => Off
      is
      begin
         return Reg'Enum_Rep;
      end Index;
   end Rep;

   -- Read a specific register
   procedure Read
     (Register : in     Registers_Index;
      Value    :    out Word32;
      Verbose  : in     Boolean := True)
   is
   begin
      Regs.Read (Value, Rep.Index (Register));

      pragma Debug (Verbose, Debug.Put (GNAT.Source_Info.Enclosing_Entity & ":  "));
      pragma Debug (Verbose, Debug.Put_Word32 (Value));
      pragma Debug (Verbose, Debug.Put (" <- "));
      pragma Debug (Verbose, Debug.Put_Word32 (Register'Enum_Rep * Register_Width));
      pragma Debug (Verbose, Debug.Put (":"));
      pragma Debug (Verbose, Debug.Put_Line (Registers_Index'Image (Register)));
   end Read;

   ----------------------------------------------------------------------------

   -- Read a specific register to post a previous write
   procedure Posting_Read (Register : Registers_Index)
   is
      Discard_Value : Word32;
   begin
      pragma Warnings
        (Off, "unused assignment to ""Discard_Value""",
         Reason => "Intentional dummy read to affect hardware.");

      Read (Register, Discard_Value);

      pragma Warnings
        (On, "unused assignment to ""Discard_Value""");
   end Posting_Read;

   ----------------------------------------------------------------------------

   -- Write a specific register
   procedure Write
     (Register : Registers_Index;
      Value    : Word32;
      Verbose  : Boolean := True)
   is
   begin
      pragma Debug (Verbose, Debug.Put (GNAT.Source_Info.Enclosing_Entity & ": "));
      pragma Debug (Verbose, Debug.Put_Word32 (Value));
      pragma Debug (Verbose, Debug.Put (" -> "));
      pragma Debug (Verbose, Debug.Put_Word32 (Register'Enum_Rep * Register_Width));
      pragma Debug (Verbose, Debug.Put (":"));
      pragma Debug (Verbose, Debug.Put_Line (Registers_Index'Image (Register)));

      Regs.Write (Rep.Index (Register), Value);
      pragma Debug (Debug.Register_Write_Wait);
   end Write;

   ----------------------------------------------------------------------------

   -- Check whether all bits in @Register@ indicated by @Mask@ are set
   procedure Is_Set_Mask
      (Register : in     Registers_Index;
       Mask     : in     Word32;
       Result   :    out Boolean)
   is
      Value : Word32;
   begin
      pragma Debug (Debug.Put (GNAT.Source_Info.Enclosing_Entity & ": "));
      pragma Debug (Debug.Put_Line (Registers_Index'Image (Register)));

      Read (Register, Value);
      Result := (Value and Mask) = Mask;

   end Is_Set_Mask;

   ----------------------------------------------------------------------------

   pragma Warnings (GNATprove, Off, "unused assignment to ""Ignored_Success""");

   -- Wait for the bits in @Register@ indicated by @Mask@ to be of @Value@
   procedure Wait
     (Register : in     Registers_Index;
      Mask     : in     Word32;
      Value    : in     Word32;
      TOut_MS  : in     Natural := Default_Timeout_MS;
      Verbose  : in     Boolean := False;
      Success  :    out Boolean)
   is
      Current : Word32;
      Timeout : Time.T;
      Timed_Out : Boolean := False;
   begin
      pragma Debug (Debug.Put (GNAT.Source_Info.Enclosing_Entity & ":  "));
      pragma Debug (Debug.Put_Word32 (Value));
      pragma Debug (Debug.Put (" <- "));
      pragma Debug (Debug.Put_Word32 (Mask));
      pragma Debug (Debug.Put (" & "));
      pragma Debug (Debug.Put_Word32 (Register'Enum_Rep * Register_Width));
      pragma Debug (Debug.Put (":"));
      pragma Debug (Debug.Put_Line (Registers_Index'Image (Register)));

      Timeout := Time.MS_From_Now (TOut_MS);
      loop
         Read (Register, Current, Verbose);
         if (Current and Mask) = Value then
            -- Ignore timeout if we succeeded anyway.
            Timed_Out := False;
            exit;
         end if;
         pragma Debug (Timed_Out, Debug.Put (GNAT.Source_Info.Enclosing_Entity));
         pragma Debug (Timed_Out, Debug.Put_Line (": Timed Out!"));
         exit when Timed_Out;

         Timed_Out := Time.Timed_Out (Timeout);
      end loop;

      Success := not Timed_Out;
   end Wait;

   procedure Wait
     (Register : Registers_Index;
      Mask     : Word32;
      Value    : Word32;
      TOut_MS  : Natural := Default_Timeout_MS;
      Verbose  : Boolean := False)
   is
      Ignored_Success : Boolean;
   begin
      Wait (Register, Mask, Value, TOut_MS, Verbose, Ignored_Success);
   end Wait;

   ----------------------------------------------------------------------------

   -- Wait for all bits in @Register@ indicated by @Mask@ to be set
   procedure Wait_Set_Mask
     (Register : in     Registers_Index;
      Mask     : in     Word32;
      TOut_MS  : in     Natural := Default_Timeout_MS;
      Verbose  : in     Boolean := False;
      Success  :    out Boolean) is
   begin
      Wait (Register, Mask, Mask, TOut_MS, Verbose, Success);
   end Wait_Set_Mask;

   procedure Wait_Set_Mask
     (Register : Registers_Index;
      Mask     : Word32;
      TOut_MS  : Natural := Default_Timeout_MS;
      Verbose  : Boolean := False)
   is
      Ignored_Success : Boolean;
   begin
      Wait (Register, Mask, Mask, TOut_MS, Verbose, Ignored_Success);
   end Wait_Set_Mask;

   ----------------------------------------------------------------------------

   -- Wait for bits in @Register@ indicated by @Mask@ to be clear
   procedure Wait_Unset_Mask
     (Register   : in     Registers_Index;
      Mask       : in     Word32;
      TOut_MS    : in     Natural := Default_Timeout_MS;
      Verbose    : in     Boolean := False;
      Success    :    out Boolean) is
   begin
      Wait (Register, Mask, 0, TOut_MS, Verbose, Success);
   end Wait_Unset_Mask;

   procedure Wait_Unset_Mask
     (Register : Registers_Index;
      Mask     : Word32;
      TOut_MS  : Natural := Default_Timeout_MS;
      Verbose  : Boolean := False)
   is
      Ignored_Success : Boolean;
   begin
      Wait (Register, Mask, 0, TOut_MS, Verbose, Ignored_Success);
   end Wait_Unset_Mask;

   ----------------------------------------------------------------------------

   -- Set bits from @Mask@ in @Register@
   procedure Set_Mask
      (Register : Registers_Index;
       Mask     : Word32)
   is
      Value : Word32;
   begin
      pragma Debug (Debug.Put (GNAT.Source_Info.Enclosing_Entity & ": "));
      pragma Debug (Debug.Put_Word32 (Mask));
      pragma Debug (Debug.Put (" .S "));
      pragma Debug (Debug.Put_Line (Registers_Index'Image (Register)));

      Read (Register, Value);
      Value := Value or Mask;
      Write (Register, Value);
   end Set_Mask;

   ----------------------------------------------------------------------------

   -- Mask out @Mask@ in @Register@
   procedure Unset_Mask
      (Register : Registers_Index;
       Mask     : Word32)
   is
      Value : Word32;
   begin
      pragma Debug (Debug.Put (GNAT.Source_Info.Enclosing_Entity & ": "));
      pragma Debug (Debug.Put_Word32 (Mask));
      pragma Debug (Debug.Put (" !S "));
      pragma Debug (Debug.Put_Line (Registers_Index'Image (Register)));

      Read (Register, Value);
      Value := Value and not Mask;
      Write (Register, Value);
   end Unset_Mask;

   ----------------------------------------------------------------------------

   -- Mask out @Unset_Mask@ and set @Set_Mask@ in @Register@
   procedure Unset_And_Set_Mask
      (Register   : Registers_Index;
       Mask_Unset : Word32;
       Mask_Set   : Word32)
   is
      Value : Word32;
   begin
      pragma Debug (Debug.Put (GNAT.Source_Info.Enclosing_Entity & ": "));
      pragma Debug (Debug.Put_Line (Registers_Index'Image (Register)));

      Read (Register, Value);
      Value := (Value and not Mask_Unset) or Mask_Set;
      Write (Register, Value);
   end Unset_And_Set_Mask;

   ----------------------------------------------------------------------------

   procedure Set_Register_Base (Base : Word64; GTT_Base : Word64 := 0)
   is
   begin
      Regs.Set_Base_Address (Base);
      if GTT_Base = 0 then
         GTT_32.Set_Base_Address
           (Base + Word64 (Config.Default_MMIO_GTT_32_Offset));
         GTT_64.Set_Base_Address (Base + Word64 (Config.MMIO_GTT_64_Offset));
      else
         GTT_32.Set_Base_Address (GTT_Base);
         GTT_64.Set_Base_Address (GTT_Base);
      end if;
   end Set_Register_Base;

end HW.GFX.GMA.Registers;
