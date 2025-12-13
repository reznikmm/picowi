--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  See https://iosoft.blog/2022/12/06/picowi_part1/ for detailed blog post.

with Interfaces;

with HAL;

with RP.Device;
with RP.Clock;
with RP.GPIO;
with RP.PIO;
with RP.PIO.Current;
with Pico;
with PIO_SPI;

procedure Picowi is
   --  WL_CLK : RP.GPIO.GPIO_Point := (Pin => 0);
   --  WL_D   : RP.GPIO.GPIO_Point := (Pin => 2);
   WL_CLK : RP.GPIO.GPIO_Point := (Pin => 29);
   WL_D   : RP.GPIO.GPIO_Point := (Pin => 24);
   WL_CS  : RP.GPIO.GPIO_Point := (Pin => 25);
   WL_ON  : RP.GPIO.GPIO_Point := (Pin => 23);

   P  : RP.PIO.PIO_Device renames RP.Device.PIO_0;
   SM : constant RP.PIO.PIO_SM := 0;

   subtype Byte is Interfaces.Unsigned_8;
   use type Byte;

   procedure Swap_Byte (Left, Right : in out Byte) is
      Temp : constant Byte := Left;
   begin
      Left := Right;
      Right := Temp;
   end Swap_Byte;

   type Byte_Array is array (Positive range <>) of Byte;

   procedure PIO_Init;

   procedure PIO_Init is

      Config : RP.PIO.PIO_SM_Config := RP.PIO.Default_SM_Config;

   begin
      P.Enable;
      P.Load (PIO_SPI.Picowi_Pio_Program_Instructions, 0);

      --  Set I/O pins to be controlled
      WL_CLK.Configure (RP.GPIO.Output, Func => P.GPIO_Function);
      WL_D.Configure (RP.GPIO.Output, Func => P.GPIO_Function);

      RP.PIO.Set_Wrap
        (Config,
         Wrap_Target => PIO_SPI.Picowi_Pio_Wrap_Target,
         Wrap        => PIO_SPI.Picowi_Pio_Wrap);

      RP.PIO.Set_Sideset (Config, 1, False, False);

      --  Configure data pin as I/O, clock pin as O/P (sideset)
      RP.PIO.Set_Out_Pins (Config, WL_D.Pin, 1);
      RP.PIO.Set_In_Pins (Config, WL_D.Pin);
      RP.PIO.Set_Sideset_Pins (Config, WL_CLK.Pin);

      --  Get 8 bits from FIFOs, disable auto-pull & auto-push
      RP.PIO.Set_Out_Shift (Config, False, False, 8);
      RP.PIO.Set_In_Shift (Config, False, False, 8);

      --  RP.PIO.Set_Clock_Frequency (Config, 1_000_000);
      RP.PIO.Set_Clkdiv_Int_Frac (Config, 1, 0);

      P.SM_Initialize (SM, 0, Config);
      P.Clear_FIFOs (SM);
      P.Set_Enabled (SM, True);
      P.Set_Pin_Direction (SM, WL_CLK.Pin, RP.PIO.Output);
      P.Set_Pin_Direction (SM, WL_D.Pin, RP.PIO.Input);
   end PIO_Init;

   procedure Write_Bits_0 (Data : Byte_Array; Bits : Positive) is
      Next : Byte := Data (Data'First);
   begin
      --  P.Set_Enabled (SM, False);
      WL_CLK.Configure (RP.GPIO.Output);
      WL_CLK.Clear;
      WL_D.Configure (RP.GPIO.Output);

      for J in 1 .. Bits loop
         if (Next and 128) /= 0 then
            WL_D.Set;
         else
            WL_D.Clear;
         end if;

         WL_CLK.Set;

         if (J mod 8 = 0) and J /= Bits then
            Next := Data (Data'First + J / 8);
         else
            Next := Interfaces.Shift_Left (Next, 1);
         end if;

         WL_CLK.Clear;
         RP.Device.Timer.Delay_Microseconds (0);
      end loop;

      WL_D.Configure (RP.GPIO.Input);
      --  WL_CLK.Configure (RP.GPIO.Output, Func => P.GPIO_Function);
      --  WL_D.Configure (RP.GPIO.Output, Func => P.GPIO_Function);
      --  P.Clear_FIFOs (SM);
      --  P.Set_Enabled (SM, True);
   end Write_Bits_0;

   procedure Write_Bits (Data : Byte_Array; Bits : Positive) is
      use type HAL.UInt5;
      use type HAL.UInt32;
   begin
      P.Clear_FIFOs (SM);
      P.Execute (SM, PIO_SPI.Offset_writer);
      P.Set_Pin_Direction (SM, WL_D.Pin, RP.PIO.Output);

      for J in 0 .. (Bits - 1) / 8 loop
         P.Put (SM, 256 * 256 * 256 * Byte'Pos (Data (Data'First + J)));
      end loop;

      while not P.TX_FIFO_Empty (SM) loop
         null;
      end loop;

      while RP.PIO.Current (P, SM) /= PIO_SPI.Offset_writer loop
         null;
      end loop;

      P.Set_Pin_Direction (SM, WL_D.Pin, RP.PIO.Input);
      P.Execute (SM, PIO_SPI.Offset_stall);
   end Write_Bits;

   procedure Read_Bits (Data : out Byte_Array; Bits : Positive) is
   begin
      P.Execute (SM, PIO_SPI.Offset_reader);
      P.Put (SM, Natural'Pos (Bits / 8 - 1));
      for J in 0 .. Bits / 8 - 1 loop
         declare
            Value : HAL.UInt32;
         begin
            P.Get (SM, Value);
            Data (Data'First + J) := Byte (Value);
         end;
      end loop;
   end Read_Bits;

   procedure Read_Bits_0 (Data : out Byte_Array; Bits : Positive) is
      Set   : Boolean;
      Index : Positive := Data'First;
   begin
      --  P.Set_Enabled (SM, False);
      WL_CLK.Configure (RP.GPIO.Output);
      WL_CLK.Clear;

      WL_D.Configure (RP.GPIO.Input);

      Data (Data'First) := 0;

      for J in 1 .. Bits loop
         Set := WL_D.Set;
         WL_CLK.Set;

         Data (Index) := Interfaces.Shift_Left (Data (Index), 1)
           + Boolean'Pos (Set);

         if (J mod 8 = 0) and J /= Bits then
            Index := Index + 1;
            Data (Index) := 0;
         end if;

         WL_CLK.Clear;
         RP.Device.Timer.Delay_Microseconds (0);
      end loop;

      --  WL_CLK.Configure (RP.GPIO.Output, Func => P.GPIO_Function);
      --  WL_D.Configure (RP.GPIO.Output, Func => P.GPIO_Function);
      --  P.Clear_FIFOs (SM);
      --  P.Set_Enabled (SM, True);
   end Read_Bits_0;

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
      Swap : Boolean := False)
   is
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

      WL_CS.Clear;

      Write_Bits (Raw, 32);

      if Data'Length < 4 then
         Write_Bits ((1 .. 4 - Data'Length => 0) & Data, 32);
      else
         Write_Bits (Data, 8 * Data'Length);
      end if;

      WL_CS.Set;
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

      WL_CS.Clear;

      Write_Bits (Raw, 32);

      if Func = DMA_1 then
         Read_Bits (Gap, 8 * Gap'Length);
      end if;

      Read_Bits (Data, 8 * Data'Length);

      WL_CS.Set;
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

   --  On chips with dual interfaces, the state of DATA2 at power-up determines
   --  which interface is to be used; for SPI, this pin must be held low,
   --  before REG_ON is set high to power up the chip.

   WL_ON.Configure (RP.GPIO.Output);
   WL_ON.Clear;  --  Power OFF

   WL_CS.Configure (RP.GPIO.Output);
   WL_CS.Set;

   WL_CLK.Configure (RP.GPIO.Output);
   WL_CLK.Clear;

   WL_D.Configure (RP.GPIO.Output);
   WL_D.Clear;

   RP.Device.Timer.Delay_Milliseconds (100);
   WL_ON.Set;  --  Power ON
   RP.Device.Timer.Delay_Milliseconds (50);

   PIO_Init;

   --  for J in 1 .. 1_000_000_000 loop
   --     Write_Bits ((16#11#, 16#33#, 16#77#, 16#0F#), 32);
   --  end loop;

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
