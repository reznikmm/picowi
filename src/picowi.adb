--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

--  See https://iosoft.blog/2022/12/06/picowi_part1/ for detailed blog post.

with RP.Device;
with RP.Clock;
with Pico;

with CYW4343X.RP_WiFi;
with CYW4343X.Generic_SPI;

procedure Picowi is

   package CYW4343X_SPI is new CYW4343X.Generic_SPI
     (Chip_Select => CYW4343X.RP_WiFi.Chip_Select,
      Read  => CYW4343X.RP_WiFi.Read_SPI,
      Write => CYW4343X.RP_WiFi.Write_SPI);

   --  procedure Write
   --    (Data : Byte_Array;
   --     Addr : Natural;
   --     Func : BCM_Function;
   --     Swap : Boolean := False)
   --  is
   --     use type Byte_Array;
   --
   --     Prefix : SPI_Message_Header :=
   --       (Length  => Data'Length,
   --        Address => Addr,
   --        Func    => Func,
   --        Incr    => True,
   --        Write   => True);
   --
   --     Raw    : Byte_Array (1 .. 4)
   --       with Import, Address => Prefix'Address;
   --  begin
   --     if Swap then
   --        Swap_Byte (Raw (1), Raw (2));
   --        Swap_Byte (Raw (3), Raw (4));
   --     end if;
   --
   --     CYW4343X.RP_WiFi.Chip_Select (On => True);
   --
   --     CYW4343X.RP_WiFi.Write_SPI (Raw);
   --
   --     if Data'Length < 4 then
   --        CYW4343X.RP_WiFi.Write_SPI ((1 .. 4 - Data'Length => 0) & Data);
   --     else
   --        CYW4343X.RP_WiFi.Write_SPI (Data);
   --     end if;
   --
   --     CYW4343X.RP_WiFi.Chip_Select (On => False);
   --  end Write;

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
      end if;
   end;

   loop
      RP.Device.Timer.Delay_Milliseconds (250);
   end loop;
end Picowi;
