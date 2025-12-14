--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  See https://iosoft.blog/2022/12/06/picowi_part1/ for detailed blog post.

with RP.Device;
with RP.Clock;
with Pico;

with CYW4343X.RP_WiFi;
with CYW4343X.Generic_IO;
with CYW4343X.Generic_SPI;

procedure Picowi is

   package CYW4343X_SPI is new CYW4343X.Generic_SPI
     (Chip_Select => CYW4343X.RP_WiFi.Chip_Select,
      Read  => CYW4343X.RP_WiFi.Read_SPI,
      Write => CYW4343X.RP_WiFi.Write_SPI);

   package CYW4343X_IO is new CYW4343X.Generic_IO
     (Read_Register => CYW4343X_SPI.Read_Register,
      Write_Register => CYW4343X_SPI.Write_Register);

begin
   RP.Clock.Initialize (Pico.XOSC_Frequency);
   RP.Device.Timer.Enable;

   CYW4343X.RP_WiFi.Configure_GPIO (Power_On => True);

   declare
      Ok : Boolean;
   begin
      for J in 1 .. 4 loop
         RP.Device.Timer.Delay_Milliseconds (2);
         CYW4343X_SPI.Detect_Chip (Ok);
         exit when Ok;
      end loop;

      if Ok then
         CYW4343X_SPI.Switch_Endian (Ok);

         if not Ok then
            raise Program_Error;
         end if;

         CYW4343X_IO.Initialize (Ok);

         if not Ok then
            raise Program_Error;
         end if;
      end if;
   end;

   loop
      RP.Device.Timer.Delay_Milliseconds (250);
   end loop;
end Picowi;
