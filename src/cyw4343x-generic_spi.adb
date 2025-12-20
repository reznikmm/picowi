--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body CYW4343X.Generic_SPI is

   package SPI_Register is
      Bus_Control : constant := 16#00#;
      Status      : constant := 16#08#;
      Read_Test   : constant := 16#14#;
   end SPI_Register;

   type SPI_Message_Header is record
      Length  : Natural range 0 .. 2**11 - 1;
      Address : Natural range 0 .. 2**17 - 1;
      Func    : Bus_Function;
      Incr    : Boolean;
      Write   : Boolean;
   end record;

   for SPI_Message_Header use record
      Length  at 0 range 0 .. 10;
      Address at 0 range 11 .. 27;
      Func    at 0 range 28 .. 29;
      Incr    at 0 range 30 .. 30;
      Write   at 0 range 31 .. 31;
   end record;

   function Swap (Value : SPI_Message_Header) return SPI_Message_Header;

   subtype Word is HAL.UInt8_Array (1 .. 4);

   function To_Bytes is new Ada.Unchecked_Conversion
     (SPI_Message_Header, Word);

   -----------------------------
   -- Available_Packet_Length --
   -----------------------------

   function Available_Packet_Length return Interfaces.Unsigned_32 is
      use type Interfaces.Unsigned_32;

      Value : Interfaces.Unsigned_32;

      PKT_AVAIL : constant := 16#100#;
      LEN_SHIFT : constant := 9;
      LEN_MASK  : constant := 16#7FF#;

   begin
      Read_Register
        (Bus_Function => CYW4343X.Bus,
         Address      => SPI_Register.Status,
         Length       => 4,
         Value        => Value);

      return
        (if (Value and PKT_AVAIL) = 0 then 0
         else Interfaces.Shift_Right (Value, LEN_SHIFT) and LEN_MASK);
   end Available_Packet_Length;

   -----------------
   -- Detect_Chip --
   -----------------

   procedure Detect_Chip (Success : out Boolean) is
      use type Interfaces.Unsigned_32;

      function Read_Register_14_Swapped return Interfaces.Unsigned_32;

      ------------------------------
      -- Read_Register_14_Swapped --
      ------------------------------

      function Read_Register_14_Swapped return Interfaces.Unsigned_32 is
         Prefix : constant SPI_Message_Header := Swap
           ((Length  => 4,
             Address => SPI_Register.Read_Test,
             Func    => Bus,
             Incr    => True,
             Write   => False));

         Raw    : constant Word := To_Bytes (Prefix);
         Result : Interfaces.Unsigned_32 := 0;
         Bytes  : HAL.UInt8_Array (1 .. 4)
           with Import, Address => Result'Address;
      begin
         Chip_Select (On => True);
         Write (Raw);
         Read (Bytes);
         Chip_Select (On => False);
         return Result;
      end Read_Register_14_Swapped;

      Value : Interfaces.Unsigned_32;
   begin
      Value := Read_Register_14_Swapped;
      Success := Value = 16#EDFE_ADBE#;
   end Detect_Chip;

   ---------------
   -- Has_Event --
   ---------------

   function Has_Event return Boolean is
      use type Interfaces.Unsigned_32;

      F2_RX_READY : constant := 16#0020#;

      Value : Interfaces.Unsigned_32;
   begin
      Read_Register
        (Bus_Function => CYW4343X.Bus,
         Address      => SPI_Register.Status,
         Length       => 1,
         Value        => Value);

      return (Value and F2_RX_READY) /= 0;
   end Has_Event;

   ----------
   -- Read --
   ----------

   procedure Read
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Value        : out HAL.UInt8_Array)
   is
      use type HAL.UInt8_Array;
      use type Interfaces.Unsigned_32;

      Prefix : constant SPI_Message_Header :=
        (Length  => Value'Length + (if Bus_Function = Backplane then 4 else 0),
         Address => Natural (Address and 16#1FFFF#),
         Func    => Bus_Function,
         Incr    => True,
         Write   => False);

      Gap    : HAL.UInt8_Array (1 .. 4);
      Raw    : constant HAL.UInt8_Array (1 .. 4) := To_Bytes (Prefix);
   begin
      Chip_Select (On => True);
      Write (Raw);

      if Bus_Function = Backplane then
         Read (Gap);
      end if;

      Read (Value);
      Chip_Select (On => False);
   end Read;

   -------------------
   -- Read_Register --
   -------------------

   procedure Read_Register
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : out Interfaces.Unsigned_32)
   is
      procedure Read
        (Data : out HAL.UInt8_Array;
         Addr : Natural;
         Func : CYW4343X.Bus_Function);

      procedure Read
        (Data : out HAL.UInt8_Array;
         Addr : Natural;
         Func : CYW4343X.Bus_Function)
      is
         Gap  : Word;

         Prefix : constant SPI_Message_Header :=
           (Length  => Length +
              (if Func = Backplane then Gap'Length else 0),
            Address => Addr,
            Func    => Func,
            Incr    => True,
            Write   => False);

         Raw    : constant Word := To_Bytes (Prefix);
      begin
         Chip_Select (On => True);
         Write (Raw);

         if Func = Backplane then
            Read (Gap);
         end if;

         Read (Data);

         Chip_Select (On => False);
      end Read;

      Result : Interfaces.Unsigned_32 := 0;
      Raw    : Word
        with Import, Address => Result'Address;
   begin
      Read (Raw, Natural (Address), Bus_Function);
      Value := Result;
   end Read_Register;

   ----------
   -- Swap --
   ----------

   function Swap (Value : SPI_Message_Header) return SPI_Message_Header is

      procedure Swap_Byte (Left, Right : in out HAL.UInt8);

      procedure Swap_Byte (Left, Right : in out HAL.UInt8) is
         Temp : constant HAL.UInt8 := Left;
      begin
         Left := Right;
         Right := Temp;
      end Swap_Byte;

      Result : SPI_Message_Header := Value;

      Raw    : HAL.UInt8_Array (1 .. 4)
        with Import, Address => Result'Address;

   begin
      Swap_Byte (Raw (1), Raw (2));
      Swap_Byte (Raw (3), Raw (4));

      return Result;
   end Swap;

   -------------------
   -- Switch_Endian --
   -------------------

   procedure Switch_Endian (Success : out Boolean) is
      use type Interfaces.Unsigned_32;

      procedure Write_Bus_Control_Register_Swapped (Bytes : Word);

      procedure Write_Bus_Control_Register_Swapped (Bytes : Word) is
         use type HAL.UInt8_Array;

         Prefix : constant SPI_Message_Header := Swap
           ((Length  => 4,
             Address => SPI_Register.Bus_Control,
             Func    => Bus,
             Incr    => True,
             Write   => True));

         Raw    : constant HAL.UInt8_Array (1 .. 8) :=
           To_Bytes (Prefix) & Bytes;
      begin
         Chip_Select (On => True);
         Write (Raw);
         Chip_Select (On => False);
      end Write_Bus_Control_Register_Swapped;

      Value : Interfaces.Unsigned_32;
   begin
      Write_Bus_Control_Register_Swapped ((04, 16#b3#, 0, 2));
      Read_Register (Bus, SPI_Register.Read_Test, 4, Value);
      Success := Value = 16#FEED_BEAD#;
   end Switch_Endian;

   -----------
   -- Write --
   -----------

   procedure Write
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Value        : HAL.UInt8_Array)
   is
      use type HAL.UInt8_Array;

      Prefix : constant SPI_Message_Header :=
        (Length  => Value'Length,
         Address => Natural (Address),
         Func    => Bus_Function,
         Incr    => True,
         Write   => True);

      Raw    : HAL.UInt8_Array (1 .. 8) :=
        To_Bytes (Prefix) & (0, 0, 0, 0);
   begin
      Chip_Select (On => True);

      if Value'Length <= 4 then
         Raw (5 .. 4 + Value'Length) := Value;
         Write (Raw);
      else
         Write (Raw (1 .. 4));
         Write (Value);
      end if;

      Chip_Select (On => False);
   end Write;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : Interfaces.Unsigned_32)
   is

      procedure Write
        (Data : Word;
         Addr : Natural;
         Func : CYW4343X.Bus_Function);

      procedure Write
        (Data : Word;
         Addr : Natural;
         Func : CYW4343X.Bus_Function)
      is
         use type HAL.UInt8_Array;

         Prefix : constant SPI_Message_Header :=
           (Length  => Length,
            Address => Addr,
            Func    => Func,
            Incr    => True,
            Write   => True);

         Raw    : constant HAL.UInt8_Array (1 .. 8) :=
           To_Bytes (Prefix) & Data;
      begin
         Chip_Select (On => True);
         Write (Raw);
         Chip_Select (On => False);
      end Write;

      Raw    : Word
        with Import, Address => Value'Address;
   begin
      Write (Raw, Natural (Address), Bus_Function);
   end Write_Register;

end CYW4343X.Generic_SPI;
