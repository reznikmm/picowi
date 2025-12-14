--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with RP.Device;
with RP.GPIO;
with RP.PIO.Current;
with RP.PIO;

with CYW4343X.RP_WiFi.PIO_SPI;

package body CYW4343X.RP_WiFi is

   WL_CLK : RP.GPIO.GPIO_Point := (Pin => 29);
   WL_D   : RP.GPIO.GPIO_Point := (Pin => 24);
   WL_CS  : RP.GPIO.GPIO_Point := (Pin => 25);
   WL_ON  : RP.GPIO.GPIO_Point := (Pin => 23);

   P  : RP.PIO.PIO_Device renames RP.Device.PIO_0;
   SM : constant RP.PIO.PIO_SM := 0;

   procedure PIO_Init;

   -----------------
   -- Chip_Select --
   -----------------

   procedure Chip_Select (On : Boolean) is
   begin
      if On then
         WL_CS.Clear;
      else
         WL_CS.Set;
      end if;
   end Chip_Select;

   --------------------
   -- Configure_GPIO --
   --------------------

   procedure Configure_GPIO (Power_On : Boolean) is
   begin
      --  On chips with dual interfaces, the state of DATA2 at power-up
      --  determines which interface is to be used; for SPI, this pin must
      --  be held low, before REG_ON is set high to power up the chip.

      WL_ON.Configure (RP.GPIO.Output);
      WL_ON.Clear;  --  Power OFF

      WL_CS.Configure (RP.GPIO.Output);
      WL_CS.Set;

      WL_CLK.Configure (RP.GPIO.Output);
      WL_CLK.Clear;

      WL_D.Configure (RP.GPIO.Output);
      WL_D.Clear;

      if Power_On then
         RP.Device.Timer.Delay_Milliseconds (100);
         RP_WiFi.Power_On;
         RP.Device.Timer.Delay_Milliseconds (50);
      end if;

      PIO_Init;
   end Configure_GPIO;

   --------------
   -- PIO_Init --
   --------------

   procedure PIO_Init is

      Config : RP.PIO.PIO_SM_Config := RP.PIO.Default_SM_Config;

   begin
      P.Enable;
      P.Load (CYW4343X.RP_WiFi.PIO_SPI.Picowi_Pio_Program_Instructions, 0);

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

   --------------
   -- Power_On --
   --------------

   procedure Power_On is
   begin
      WL_ON.Set;  --  Power ON
   end Power_On;

   procedure Read_SPI (Data : out HAL.UInt8_Array) is
      use type HAL.UInt32;
   begin
      P.Execute (SM, CYW4343X.RP_WiFi.PIO_SPI.Offset_reader);
      P.Put (SM, Data'Length - 1);
      for Item of Data loop
         declare
            Value : HAL.UInt32;
         begin
            P.Get (SM, Value);
            Item := HAL.UInt8 (Value);
         end;
      end loop;
   end Read_SPI;

   ---------------
   -- Write_SPI --
   ---------------

   procedure Write_SPI (Data : HAL.UInt8_Array) is
      use type HAL.UInt5;
      use type HAL.UInt32;
   begin
      P.Clear_FIFOs (SM);
      P.Execute (SM, PIO_SPI.Offset_writer);
      P.Set_Pin_Direction (SM, WL_D.Pin, RP.PIO.Output);

      for Item of Data loop
         P.Put (SM, 256 * 256 * 256 * HAL.UInt32 (Item));
      end loop;

      while not P.TX_FIFO_Empty (SM) loop
         null;
      end loop;

      while RP.PIO.Current (P, SM) /= PIO_SPI.Offset_writer loop
         null;
      end loop;

      P.Set_Pin_Direction (SM, WL_D.Pin, RP.PIO.Input);
      P.Execute (SM, PIO_SPI.Offset_stall);
   end Write_SPI;

end CYW4343X.RP_WiFi;
