pragma Ada_2022;

with RP.Timer;
with CYW4343X.Generic_IO;
with CYW4343X.Generic_SPI;
with CYW4343X.RP_WiFi;

package Net.Interfaces.SDPCM is

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
      Is_Ready_To_Send        => CYW4343X_SPI.Is_Ready_To_Send,
      SSID                    => SSID,
      Password                => Password,
      Security_Mode           => CYW4343X.WPA2_AES,
      Time                    => RP.Timer.Time,
      Timeout                 => Timeout,
      Is_Expired              => Is_Expired);

   type SDPCM_Ifnet is new Ifnet_Type with record
      State : CYW4343X_IO.Joining_State;
   end record;

   procedure Create (Self : in out SDPCM_Ifnet'Class);

   procedure Send
     (Self   : in out SDPCM_Ifnet;
      Packet : in out Net.Buffers.Buffer_Type);
   --  How to synchronize with Generic_Receiver task???
   --  How to handle data during restart join???

   procedure Receive
     (Self   : in out SDPCM_Ifnet;
      Packet : in out Net.Buffers.Buffer_Type);

end Net.Interfaces.SDPCM;
