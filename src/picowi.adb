--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  See https://iosoft.blog/2022/12/06/picowi_part1/ for detailed blog post.

pragma Ada_2022;

with Net.Interfaces.SDPCM;
with RP.Device;
with RP.Clock;
with Pico;

with CYW4343X.Firmware_43439;
with CYW4343X.RP_WiFi;

with Net.Buffers;
with Net.Protos.Dispatchers;
with Net.Protos.Arp;
with Net.Protos.Icmp;
with Net.Protos.IPv4;

with Network;

procedure Picowi is

   package CYW4343X_SPI renames Net.Interfaces.SDPCM.CYW4343X_SPI;
   package CYW4343X_IO renames Net.Interfaces.SDPCM.CYW4343X_IO;

   State : CYW4343X_IO.Joining_State renames Network.MAC.State;
   Seq   : Net.Uint16 := 0;

   procedure Send_Ping (Host : Net.Ip_Addr; Seq : in out Net.Uint16);

   procedure Send_Ping (Host : Net.Ip_Addr; Seq : in out Net.Uint16) is
      Packet : Net.Buffers.Buffer_Type;
      Status : Net.Error_Code;
   begin

      Net.Buffers.Allocate (Packet);

      if not Packet.Is_Null then
         Packet.Set_Length (64);
         Net.Protos.Icmp.Echo_Request
           (Ifnet     => Network.LAN.all,
            Target_Ip => Host,
            Packet    => Packet,
            Seq       => Seq,
            Ident     => 1234,
            Status    => Status);

         Seq := Net.Uint16'Succ (Seq);
      end if;
   end Send_Ping;

begin
   RP.Clock.Initialize (Pico.XOSC_Frequency);
   RP.Device.Timer.Enable;

   CYW4343X.RP_WiFi.Configure_GPIO (Power_On => True);

   declare
      Ok : Boolean;
      MAC : CYW4343X_IO.Ether_Addr;
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
           (State,
            CYW4343X.Firmware_43439.Firmware_Image,
            CYW4343X.Firmware_43439.NVRAM_Image,
            CYW4343X.Firmware_43439.CLM_Data_Image,
            MAC,
            Ok);

         if not Ok then
            raise Program_Error;
         end if;

         CYW4343X_IO.Start_Join (State, Ok);
         pragma Assert (Ok);
      end if;
   end;

   loop
      CYW4343X_IO.Event_Poll (State);
      RP.Device.Timer.Delay_Milliseconds (1);

      if CYW4343X_IO.Is_Joined (State) then
         CYW4343X_IO.Turn_LED (True);
         exit;
      end if;
   end loop;

   Network.Initialize;

   declare
      Ignore : Net.Protos.Receive_Handler;
   begin
      Net.Protos.Dispatchers.Set_Handler
        (Proto    => Net.Protos.IPv4.P_ICMP,
         Handler  => Network.ICMP_Handler'Access,
         Previous => Ignore);
   end;

   loop
      Net.Protos.Arp.Timeout (Network.LAN.all);

      Send_Ping (Network.LAN.Gateway, Seq);

      RP.Device.Timer.Delay_Milliseconds (1000);
   end loop;
end Picowi;
