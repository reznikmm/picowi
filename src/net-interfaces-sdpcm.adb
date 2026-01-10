with Net.Headers;
with Net.Protos.IPv4;
with HAL;

package body Net.Interfaces.SDPCM is

   type Uint16_Array is array (Positive range <>) of Uint16;

   function Get_Ip_Sum (Raw : Uint16_Array) return Uint16;

   procedure Create (Self : in out SDPCM_Ifnet'Class) is null;

   ----------------
   -- Get_Ip_Sum --
   ----------------

   function Get_Ip_Sum (Raw : Uint16_Array) return Uint16 is
      Sum : Uint32 := 0;
   begin
      for Item of Raw loop
         Sum := Sum + Uint32 (Item);
      end loop;

      while Sum >= 16#1_0000# loop
         Sum := Sum - 16#1_0000# + 1;
      end loop;

      return not Uint16 (Sum);
   end Get_Ip_Sum;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Self   : in out SDPCM_Ifnet;
      Packet : in out Net.Buffers.Buffer_Type)
   is
      Addr  : constant System.Address := Packet.Get_Data_Address;
      Size  : constant Natural := Net.Buffers.Data_Type'Length;

      Raw   : HAL.UInt8_Array (1 .. Size)
        with Import, Address => Addr;
      Last : Natural;
   begin
      CYW4343X_IO.Receive (Self.State, Raw, Last);
      Packet.Set_Length (Uint16 (Last));
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Self   : in out SDPCM_Ifnet;
      Packet : in out Net.Buffers.Buffer_Type)
   is
      pragma Unreferenced (Self);

      Addr   : constant System.Address := Packet.Get_Data_Address;
      Ether  : Net.Headers.Ether_Header renames Packet.Ethernet.all;
      Ip     : Net.Headers.IP_Header renames Packet.IP.all;
      Ip_Raw : Uint16_Array (1 .. 10) with Import, Address => Ip'Address;
      Size   : constant Natural := Natural (Packet.Get_Length);
   begin
      if Ether.Ether_Type = Headers.To_Network (Net.Protos.ETHERTYPE_IP) then
         --  IPv4 header checksum offload
         Ip.Ip_Sum := 0;
         Ip.Ip_Sum := Get_Ip_Sum (Ip_Raw);

         case Ip.Ip_P is
            when Net.Protos.IPv4.P_ICMP =>
               declare
                  Size     : constant Positive := Positive
                    (Packet.Get_Data_Size (Net.Buffers.IP_PACKET)) / 2;

                  ICMP     : Net.Headers.ICMP_Header renames Packet.ICMP.all;

                  ICMP_Raw : Uint16_Array (1 .. Size)
                    with Import, Address => ICMP'Address;
               begin
                  Packet.ICMP.Icmp_Checksum := 0;
                  Packet.ICMP.Icmp_Checksum := Get_Ip_Sum (ICMP_Raw);
               end;

            when Net.Protos.IPv4.P_UDP =>
               Packet.UDP.Uh_Sum := 0;

            when others =>
               null;
         end case;
      end if;

      --  TBD: Wait SPI_STATUS_F2_RX_READY
      declare
         Raw : HAL.UInt8_Array (1 .. Size)
           with Import, Address => Addr;
      begin
         CYW4343X_IO.Send (Raw);
      end;
   end Send;

end Net.Interfaces.SDPCM;
