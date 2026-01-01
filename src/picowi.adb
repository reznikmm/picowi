--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  See https://iosoft.blog/2022/12/06/picowi_part1/ for detailed blog post.

pragma Ada_2022;

with RP.Device;
with RP.Clock;
with RP.Timer;
with Pico;

with CYW4343X.Firmware_43439;
with CYW4343X.Generic_IO;
with CYW4343X.Generic_SPI;
with CYW4343X.RP_WiFi;

procedure Picowi is

   function SSID     return String is ("guest");
   function Password return String is ("guest123");

   use type RP.Timer.Time;

   function Timeout (Second : Natural) return RP.Timer.Time is
     (RP.Timer.Milliseconds (Second * 1000) + RP.Timer.Clock);

   function Is_Expired (Timeout : RP.Timer.Time) return Boolean is
     (Timeout < RP.Timer.Clock);

   package CYW4343X_SPI is new CYW4343X.Generic_SPI
     (Chip_Select => CYW4343X.RP_WiFi.Chip_Select,
      Read        => CYW4343X.RP_WiFi.Read_SPI,
      Write       => CYW4343X.RP_WiFi.Write_SPI);

   package CYW4343X_IO is new CYW4343X.Generic_IO
     (Read_Register           => CYW4343X_SPI.Read_Register,
      Write_Register          => CYW4343X_SPI.Write_Register,
      Read                    => CYW4343X_SPI.Read,
      Write_Prefix_Length     => CYW4343X_SPI.Output_Prefix'Length,
      Write_Prefix            => CYW4343X_SPI.Write_Prefix,
      Write                   => CYW4343X_SPI.Write,
      Has_Event               => CYW4343X_SPI.Has_Event,
      Clear_Error             => CYW4343X_SPI.Clear_Error,
      Available_Packet_Length => CYW4343X_SPI.Available_Packet_Length,
      SSID                    => SSID,
      Password                => Password,
      Security_Mode           => CYW4343X.WPA2_AES,
      Time                    => RP.Timer.Time,
      Timeout                 => Timeout,
      Is_Expired              => Is_Expired);

   State : CYW4343X_IO.Joining_State;

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

         CYW4343X_IO.Initialize
           (CYW4343X.Firmware_43439.Firmware_Image,
            CYW4343X.Firmware_43439.NVRAM_Image,
            CYW4343X.Firmware_43439.CLM_Data_Image,
            Ok);

         if not Ok then
            raise Program_Error;
         end if;

         CYW4343X_IO.Start_Join (Ok);
         pragma Assert (Ok);
      end if;
   end;

   loop
      CYW4343X_IO.Event_Poll (State);
      RP.Device.Timer.Delay_Milliseconds (1);
   end loop;
end Picowi;
