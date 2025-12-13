--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  See https://iosoft.blog/2022/12/06/picowi_part1/ for detailed blog post.

with Interfaces;

with HAL;

with RP.Device;
with RP.Clock;
with Pico;

with CYW4343X.RP_WiFi;

procedure Picowi is

   subtype Byte is HAL.UInt8;

   procedure Swap_Byte (Left, Right : in out Byte) is
      Temp : constant Byte := Left;
   begin
      Left := Right;
      Right := Temp;
   end Swap_Byte;

   subtype Byte_Array is HAL.UInt8_Array;

   type BCM_Function is
     (SPI_Register,  --  All SPI specific registers
      Other_Register,
      --  Registers and memories belonging to other blocks in the chip (64
      --  bytes max)
      DMA_1,   --  DMA channel 1. WLAN packets up to 2048 bytes.
      DMA_2);  --  DMA channel 2 (optional). Packets up to 2048 byte

   type SPI_Message_Header is record
      Length  : Natural range 0 .. 2**11 - 1;
      Address : Natural range 0 .. 2**17 - 1;
      Func    : BCM_Function;
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

   procedure Write
     (Data : Byte_Array;
      Addr : Natural;
      Func : BCM_Function;
      Swap : Boolean := False);

   procedure Read
     (Data : out Byte_Array;
      Addr : Natural;
      Func : BCM_Function;
      Swap : Boolean := False);

   function Read_Register
     (Addr : Natural;
      Func : BCM_Function;
      Len  : Natural;
      Swap : Boolean := False) return Interfaces.Unsigned_32;

   procedure Write
     (Data : Byte_Array;
      Addr : Natural;
      Func : BCM_Function;
      Swap : Boolean := False)
   is
      use type Byte_Array;

      Prefix : SPI_Message_Header :=
        (Length  => Data'Length,
         Address => Addr,
         Func    => Func,
         Incr    => True,
         Write   => True);

      Raw    : Byte_Array (1 .. 4)
        with Import, Address => Prefix'Address;
   begin
      if Swap then
         Swap_Byte (Raw (1), Raw (2));
         Swap_Byte (Raw (3), Raw (4));
      end if;

      CYW4343X.RP_WiFi.CS_Clear;

      CYW4343X.RP_WiFi.Write_SPI (Raw);

      if Data'Length < 4 then
         CYW4343X.RP_WiFi.Write_SPI ((1 .. 4 - Data'Length => 0) & Data);
      else
         CYW4343X.RP_WiFi.Write_SPI (Data);
      end if;

      CYW4343X.RP_WiFi.CS_Set;
   end Write;

   procedure Read
     (Data : out Byte_Array;
      Addr : Natural;
      Func : BCM_Function;
      Swap : Boolean := False)
   is
      Gap  : Byte_Array (1 .. 4);

      Prefix : SPI_Message_Header :=
        (Length  => Data'Length +
           (if Func = DMA_1 then Gap'Length else 0),
         Address => Addr,
         Func    => Func,
         Incr    => True,
         Write   => False);

      Raw    : Byte_Array (1 .. 4)
        with Import, Address => Prefix'Address;
   begin
      if Swap then
         Swap_Byte (Raw (1), Raw (2));
         Swap_Byte (Raw (3), Raw (4));
      end if;

      CYW4343X.RP_WiFi.CS_Clear;

      CYW4343X.RP_WiFi.Write_SPI (Raw);

      if Func = DMA_1 then
         CYW4343X.RP_WiFi.Read_SPI (Gap);
      end if;

      CYW4343X.RP_WiFi.Read_SPI (Data);

      CYW4343X.RP_WiFi.CS_Set;
   end Read;

   function Read_Register
     (Addr : Natural;
      Func : BCM_Function;
      Len  : Natural;
      Swap : Boolean := False) return Interfaces.Unsigned_32
   is
      Result : Interfaces.Unsigned_32 := 0;
      Raw    : Byte_Array (1 .. 4)
        with Import, Address => Result'Address;
   begin
      Read (Raw (1 .. Len), Addr, Func, Swap);
      return Result;
   end Read_Register;

begin
   RP.Clock.Initialize (Pico.XOSC_Frequency);
   RP.Device.Timer.Enable;

   CYW4343X.RP_WiFi.Configure_GPIO (Power_On => True);

   declare
      Ok : Boolean;
   begin
      for J in 1 .. 4 loop
         declare
            use type Interfaces.Unsigned_32;
            Value : Interfaces.Unsigned_32;
         begin
            RP.Device.Timer.Delay_Milliseconds (2);
            Value := Read_Register (16#14#, SPI_Register, 4, True);
            Ok := Value = 16#EDFE_ADBE#;
            exit when Ok;
         end;
      end loop;

      if Ok then
         Write ((04, 16#b3#, 0, 2), 0, SPI_Register, True);

         declare
            use type Interfaces.Unsigned_32;
            Value : Interfaces.Unsigned_32;
         begin
            Value := Read_Register (16#14#, SPI_Register, 4);
            if Value /= 16#FEED_BEAD# then
               raise Program_Error;
            end if;
         end;
      end if;
   end;

   loop
      RP.Device.Timer.Delay_Milliseconds (250);
   end loop;
end Picowi;
