--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Unchecked_Conversion;

with RP.Device;

package body CYW4343X.Generic_IO is

   type Input_Buffer is new HAL.UInt8_Array
     with Alignment => 4;

   Input : Input_Buffer (1 .. 1536);

   package Backplane_Register is
      Win_Addr : constant := 16#1000a#;  --  Window addr
      Chip_Clock_CSR : constant := 16#1000e#;  --  Chip clock ctrl
      Pull_Up        : constant := 16#1000f#;
      Sleep_CSR      : constant := 16#1001f#;
   end Backplane_Register;

   subtype Frame_Tag is Interfaces.Unsigned_32;

   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;

   function Make_Tag (Length : Interfaces.Unsigned_16) return Frame_Tag is
     (Interfaces.Shift_Left (Interfaces.Unsigned_32 (not Length), 16) +
      Interfaces.Unsigned_32 (Length));

   package Executor is

      type Step_Kind is
        (Write_Register,
         Write_Register_Variable,
         Read_Register,
         Read_Register_Until,
         Upload_Firmware,
         Wait_Any_Event,
         Clear_Error,
         Sleep);

      type Step (Kind : Step_Kind := Sleep) is record
         case Kind is
            when Read_Register
               | Read_Register_Until
               | Write_Register
               | Write_Register_Variable
            =>
               Bus_Function : CYW4343X.Bus_Function;
               Address      : Interfaces.Unsigned_32;
               Length       : Positive;

               case Kind is
                  when Read_Register | Write_Register_Variable =>
                     null;

                  when Read_Register_Until =>
                     Mask : Interfaces.Unsigned_32;
                     Trys : Natural;

                  when Write_Register =>
                     Value : Interfaces.Unsigned_32;

                  when Upload_Firmware
                     | Wait_Any_Event
                     | Clear_Error
                     | Sleep
                   =>
                     null;
               end case;

            when Upload_Firmware =>
               Firmware : Positive;

            when Wait_Any_Event | Clear_Error =>
               null;

            when Sleep =>
               Milliseconds : Natural;
         end case;
      end record;

      type Step_Array is array (Positive range <>) of Step;

      procedure Execute
        (Steps        : Step_Array;
         Success      : in out Boolean;
         Firmware_1   : HAL.UInt8_Array;
         Firmware_2   : HAL.UInt8_Array;
         Custom_Value : Interfaces.Unsigned_32 := 0);

      generic
         Use_RAM : Boolean;
      package Reset is
         ARM_Core_Addr : constant Interfaces.Unsigned_32 := 16#1810_3000#;
         RAM_Core_Addr : constant Interfaces.Unsigned_32 := 16#1810_4000#;
         AI_IOCTRL_OSET : constant := 16#408#;
         AI_RESETCTRL_OSET : constant := 16#800#;
         Base : constant Interfaces.Unsigned_32 :=
           (if Use_RAM then RAM_Core_Addr else ARM_Core_Addr) - 16#1810_0000#;

         List : constant Step_Array :=
           [1 =>
              (Kind         => Write_Register,
               Bus_Function => CYW4343X.Backplane,
               Address      => Backplane_Register.Win_Addr,
               Value        => 16#1810_0000# / 256,
               Length       => 3),
            2 =>
              (Kind         => Read_Register,
               Bus_Function => CYW4343X.Backplane,
               Address      => Base + AI_IOCTRL_OSET,
               Length       => 1),
            3 =>
              (Kind         => Write_Register,
               Bus_Function => CYW4343X.Backplane,
               Address      => Base + AI_IOCTRL_OSET,
               Value        => 3,
               Length       => 1),
            4 =>
              (Kind         => Read_Register,
               Bus_Function => CYW4343X.Backplane,
               Address      => Base + AI_IOCTRL_OSET,
               Length       => 1),
            5 =>
              (Kind         => Write_Register,
               Bus_Function => CYW4343X.Backplane,
               Address      => Base + AI_RESETCTRL_OSET,
               Value        => 0,
               Length       => 1),
            6 =>
              (Kind     => Sleep,
               Milliseconds => 1),
            7 =>
              (Kind         => Write_Register,
               Bus_Function => CYW4343X.Backplane,
               Address      => Base + AI_IOCTRL_OSET,
               Value        => 1,
               Length       => 1),
            8 =>
              (Kind         => Read_Register,
               Bus_Function => CYW4343X.Backplane,
               Address      => Base + AI_IOCTRL_OSET,
               Length       => 1),
            9 =>
              (Kind     => Sleep,
               Milliseconds => 1)];
      end Reset;

      Join_Start : constant Step_Array :=
        [1 =>
           (Kind         => Write_Register,  --  Clear pullups
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Pull_Up,
            Value        => 16#0F#,
            Length       => 1),
         2 =>
           (Kind         => Write_Register,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Pull_Up,
            Value        => 0,
            Length       => 1),
         3 =>
           (Kind         => Read_Register,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Pull_Up,
            Length       => 1),
         4 => (Kind => Clear_Error),
      --  Set sleep KSO (should poll to check for success)
         5 =>
           (Kind         => Write_Register,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Sleep_CSR,
            Value        => 1,
            Length       => 1),
         6 =>
           (Kind         => Write_Register,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Sleep_CSR,
            Value        => 1,
            Length       => 1),
         7 =>
           (Kind         => Read_Register,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Sleep_CSR,
            Length       => 1)];

   end Executor;

   procedure Upload_Firmware
     (Address : Interfaces.Unsigned_32;
      Data    : HAL.UInt8_Array);

   procedure CLM_Load
     (State : in out Joining_State;
      Data  : HAL.UInt8_Array);

   procedure Get_Response
     (Data   : out Input_Buffer;
      Last   : out Natural);

   procedure Restart_Join (State : in out Joining_State);
   procedure Stop_Join (State : in out Joining_State);

   procedure Change_State (State : in out Joining_State);

   package Events is

      type Event is new Interfaces.Unsigned_8;

      function JOIN return Event is (1);
      --  1, /** differentiates join IBSS from found (WLC_E_START) IBSS */
      function ASSOC return Event is (7);
      --  7, /** 802.11 ASSOC request */
      function REASSOC return Event is (9);
      --  9, /** 802.11 REASSOC request */
      function ASSOC_REQ_IE return Event is (87);
      function ASSOC_RESP_IE return Event is (88);
      function SET_SSID return Event is (0);
      --  0  /** indicates status of set SSID */,
      function LINK return Event is (16);
      --  16, /** generic link indication */
      function AUTH return Event is (3);
      --  3, /** 802.11 AUTH request */
      function PSK_SUP return Event is (46);
      --  46, /** WPA Handshake */
      function EAPOL_MSG return Event is (25);
      --  25, /** Event encapsulating an EAPOL message */
      function DISASSOC_IND return Event is (12);
      --  12, /** 802.11 DISASSOC indication */

      function Last_Event return Event is (208);

      type Event_Mask is array (Event range 0 .. Last_Event) of Boolean
        with Pack, Alignment => 1;

      type Event_Array is array (Positive range <>) of Event;

      Join_Events : constant Event_Array :=
        [JOIN,
         ASSOC,
         REASSOC,
         ASSOC_REQ_IE,
         ASSOC_RESP_IE,
         SET_SSID,
         LINK,
         AUTH,
         PSK_SUP,
         EAPOL_MSG,
         DISASSOC_IND];

      function To_Mask (List : Event_Array) return Event_Mask;

      subtype Raw_Event_Mask is HAL.UInt8_Array (1 .. Event_Mask'Size / 8 + 4);

      function To_Raw_Event_Mask (Mask : Event_Mask) return Raw_Event_Mask;

   end Events;

   package body Events is

      -------------
      -- To_Mask --
      -------------

      function To_Mask (List : Event_Array) return Event_Mask is
         Mask : Event_Mask := [others => False];
      begin
         for Event of List loop
            Mask (Event) := True;
         end loop;

         return Mask;
      end To_Mask;

      -----------------------
      -- To_Raw_Event_Mask --
      -----------------------

      function To_Raw_Event_Mask (Mask : Event_Mask) return Raw_Event_Mask is
         Raw : Raw_Event_Mask := [others => 0];

         Copy : Event_Mask
           with Import, Address => Raw (5)'Address;
      begin
         Copy := Mask;

         return Raw;
      end To_Raw_Event_Mask;

   end Events;

   package IOCTL is
      type Command is new Interfaces.Unsigned_32;

      WLC_UP           : constant Command := 2;
      WLC_DOWN         : constant Command := 3;
      WLC_SET_INFRA    : constant Command := 20;
      WLC_SET_AUTH     : constant Command := 22;
      WLC_SET_SSID     : constant Command := 26;
      WLC_SET_ANTDIV   : constant Command := 64;
      WLC_SET_GMODE    : constant Command := 110;
      WLC_SET_WSEC     : constant Command := 134;
      WLC_SET_BAND     : constant Command := 142;
      WLC_SET_WPA_AUTH : constant Command := 165;
      WLC_SET_VAR      : constant Command := 263;
      WLC_SET_WSEC_PMK : constant Command := 268;

      procedure Set
        (State   : in out Joining_State;
         Command : IOCTL.Command;
         Name    : HAL.UInt8_Array;
         Data    : HAL.UInt8_Array);

      procedure Set
        (State   : in out Joining_State;
         Command : IOCTL.Command;
         Name    : String;
         Data    : HAL.UInt8_Array);
      --  TBD: Timeout parameter, success result

      procedure Set
        (State   : in out Joining_State;
         Command : IOCTL.Command;
         Name    : String;
         Data    : Interfaces.Unsigned_32);

      procedure Get
        (State   : in out Joining_State;
         Command : IOCTL.Command := 262;
         Name    : HAL.UInt8_Array;
         Data    : out HAL.UInt8_Array);

      type IOCTL_Header is record
         Command    : IOCTL.Command;
         Out_Length : Interfaces.Unsigned_16;
         In_Length  : Interfaces.Unsigned_16;
         Flags      : Interfaces.Unsigned_32;
         Status     : Interfaces.Unsigned_32;
      end record;

      for IOCTL_Header use record
         Command    at 0 range 0 .. 31;
         Out_Length at 4 range 0 .. 15;
         In_Length  at 6 range 0 .. 15;
         Flags      at 8 range 0 .. 31;
         Status     at 12 range 0 .. 31;
      end record;

   end IOCTL;

   package SDPCM is
      type BDC_Header is record
         Flags    : Interfaces.Unsigned_8;
         Priority : Interfaces.Unsigned_8;
         Flags2   : Interfaces.Unsigned_8;
         Offset   : Natural range 0 .. 255;
      end record;

      for BDC_Header use record
         Flags    at 0 range 0 .. 7;
         Priority at 1 range 0 .. 7;
         Flags2   at 2 range 0 .. 7;
         Offset   at 3 range 0 .. 7;
      end record;

      function Is_Valid (Tag : Frame_Tag) return Boolean is
        (((Interfaces.Shift_Right (Tag, 16) xor Tag) and 16#FFFF#) = 16#FFFF#);

      type SDPCM_Channel is new Interfaces.Unsigned_8;

      Control : constant SDPCM_Channel := 0;
      Event   : constant SDPCM_Channel := 1;
      Data    : constant SDPCM_Channel := 2;

      subtype Event_Or_Data is SDPCM_Channel range Event .. Data;

      type SDPCM_Header is record
         Tag      : Frame_Tag;
         Sequence : Interfaces.Unsigned_8;
         Channel  : SDPCM_Channel;
         Next_Len : Interfaces.Unsigned_8;
         Hdr_Len  : Natural range 0 .. 255;  --  SDPCM header plus any padding
         Flow     : Interfaces.Unsigned_8;
         Credit   : Interfaces.Unsigned_8;
         Reserved : Interfaces.Unsigned_16;
      end record;

      for SDPCM_Header use record
         Tag      at 0 range 0 .. 31;
         Sequence at 4 range 0 .. 7;
         Channel  at 5 range 0 .. 7;
         Next_Len at 6 range 0 .. 7;
         Hdr_Len  at 7 range 0 .. 7;
         Flow     at 8 range 0 .. 7;
         Credit   at 9 range 0 .. 7;
         Reserved at 10 range 0 .. 15;
      end record;

      type Packet (Channel : SDPCM_Channel := 0) is record
         case Channel is
            when Control =>
               IOCTL_Header : IOCTL.IOCTL_Header;
               IOCTL_Offset : Natural;
            when Event | Data =>
               Offset       : Natural;
            when others =>
               null;
         end case;
      end record;

      procedure Event_Poll
        (State  : in out Joining_State;
         Input  : in out Input_Buffer;
         Packet : out SDPCM.Packet;
         To     : out Natural);

      procedure Decode_Input
        (Input  : Input_Buffer;
         Result : out Packet);

   end SDPCM;

   package body SDPCM is

      ------------------
      -- Decode_Input --
      ------------------

      procedure Decode_Input
        (Input  : Input_Buffer;
         Result : out Packet)
      is
         subtype IOCTL_Header_Raw is
           Input_Buffer (1 .. IOCTL.IOCTL_Header'Size / 8);

         function To_IOCTL_Header is new Ada.Unchecked_Conversion
           (IOCTL_Header_Raw, IOCTL.IOCTL_Header);

         subtype BDC_Header_Raw is
           Input_Buffer (1 .. BDC_Header'Size / 8);

         function To_BDC_Header is new Ada.Unchecked_Conversion
           (BDC_Header_Raw, BDC_Header);

         SDPCM : SDPCM_Header
           with Import, Address => Input'Address;
      begin
         if not Is_Valid (SDPCM.Tag) or else
           (SDPCM.Channel = Control and then
            Input'Last < SDPCM.Hdr_Len + IOCTL.IOCTL_Header'Size / 8)
           or else
           (SDPCM.Channel in Event | Data and then
            Input'Last < SDPCM.Hdr_Len + BDC_Header'Size / 8)
         then
            Result := (Channel => SDPCM_Channel'Last);
         elsif SDPCM.Channel = Control then
            declare
               Skip : constant Natural := SDPCM.Hdr_Len;

               Header : constant IOCTL.IOCTL_Header := To_IOCTL_Header
                 (Input (Skip + 1 .. Skip + IOCTL.IOCTL_Header'Size / 8));

               Header_Length : constant Positive :=
                 Skip + IOCTL.IOCTL_Header'Size / 8;

            begin
               Result := (Control, Header, Header_Length + 1);
            end;
         elsif SDPCM.Channel in Event .. Data then
            declare
               Skip : constant Natural := SDPCM.Hdr_Len;

               BDC : constant BDC_Header := To_BDC_Header
                 (Input (Skip + 1 .. Skip + BDC_Header'Size / 8));

               Header_Length : constant Positive :=
                 Skip + BDC'Size / 8 + 4 * BDC.Offset;

            begin
               if Input'Last >= Header_Length + 1 then
                  Result :=
                    (Channel => Event_Or_Data (SDPCM.Channel),
                     Offset  => Header_Length + 1);
               end if;
            end;
         end if;
      end Decode_Input;

      type ETHER_HDR is record
         Dest_Addr : HAL.UInt8_Array (1 .. 6);
         Srce_Addr : HAL.UInt8_Array (1 .. 6);
         Tipe      : Interfaces.Unsigned_16;
      end record
        with Pack;

      type BCMETH_HDR is record
         Subtipe     : Interfaces.Unsigned_16;
         Len         : Interfaces.Unsigned_16;
         Ver         : Interfaces.Unsigned_8;
         Oui         : HAL.UInt8_Array (1 .. 3);
         Usr_Subtype : Interfaces.Unsigned_16;
      end record
        with Pack;

      type EVENT_HDR is record
         Ver        : Interfaces.Unsigned_16;
         Flags      : Interfaces.Unsigned_16;
         Event_Type : Interfaces.Unsigned_32;
         Status     : Interfaces.Unsigned_32;
         Reason     : Interfaces.Unsigned_32;
         Auth_Type  : Interfaces.Unsigned_32;
         Datalen    : Interfaces.Unsigned_32;
         Addr       : HAL.UInt8_Array (1 .. 6);
         Ifname     : HAL.UInt8_Array (1 .. 16);
         Ifidx      : Interfaces.Unsigned_8;
         Bsscfgidx  : Interfaces.Unsigned_8;
      end record
        with Pack;

      type Event_Record is record
         Ether  : ETHER_HDR;
         Bcmeth : BCMETH_HDR;
         Eventh : EVENT_HDR;
      end record
        with Pack;

      procedure On_Event
        (State : in out Joining_State;
         Event : Event_Record);

      ----------------
      -- Event_Poll --
      ----------------

      procedure Event_Poll
        (State  : in out Joining_State;
         Input  : in out Input_Buffer;
         Packet : out SDPCM.Packet;
         To     : out Natural)
      is
         Event_Length : constant Positive := Event_Record'Size / 8;
         subtype Event_Record_Raw is HAL.UInt8_Array (1 .. Event_Length);

         function To_Event_Record is new Ada.Unchecked_Conversion
           (Event_Record_Raw, Event_Record);

         Event : Event_Record;
      begin
         Get_Response (Input, To);

         if To = 0 then
            Packet := (Channel => SDPCM.SDPCM_Channel'Last);
         else
            Decode_Input (Input (1 .. To), Packet);

            if Packet.Channel = SDPCM.Event
              and then To - Packet.Offset + 1 >= Event_Length
            then
               Event := To_Event_Record
                 (HAL.UInt8_Array
                   (Input
                     (Packet.Offset .. Packet.Offset + Event_Length - 1)));

               On_Event (State, Event);
            end if;
         end if;
      end Event_Poll;

      procedure On_Event
        (State : in out Joining_State;
         Event : Event_Record)
      is
         WLC_E_LINK         : constant := 16#10_00_00_00#;  --  SWAP32 (16)
         WLC_E_PSK_SUP      : constant := 16#2e_00_00_00#;  --  SWAP32 (46)
         WLC_E_DISASSOC_IND : constant := 16#0c_00_00_00#;  --  SWAP32 (12)
      begin
         if Event.Eventh.Event_Type = WLC_E_LINK
           and Event.Eventh.Status = 0
         then
            State.Link_Up := (Event.Eventh.Flags and 16#01_00#) /= 0;
         elsif Event.Eventh.Event_Type = WLC_E_PSK_SUP then
            State.Link_Auth := Event.Eventh.Status = 16#06_00_00_00#;
         elsif Event.Eventh.Event_Type = WLC_E_DISASSOC_IND then
            State.Link_Up := False;
            State.Link_Auth := False;
            State.Link_Fail := True;
         end if;
      end On_Event;

   end SDPCM;

   package body Executor is

      procedure Execute
        (Steps        : Step_Array;
         Success      : in out Boolean;
         Firmware_1   : HAL.UInt8_Array;
         Firmware_2   : HAL.UInt8_Array;
         Custom_Value : Interfaces.Unsigned_32 := 0)
      is
         Value : Interfaces.Unsigned_32;
         Index : Positive := Steps'First;
      begin
         if not Success then
            return;
         end if;

         while Index in Steps'Range loop
            declare
               Step : constant Executor.Step := Steps (Index);
            begin
               case Step.Kind is
                  when Read_Register =>
                     Read_Register
                       (Bus_Function => Step.Bus_Function,
                        Address      => Step.Address,
                        Length       => Step.Length,
                        Value        => Value);

                  when Read_Register_Until =>
                     for J in 1 .. Step.Trys loop
                        Read_Register
                          (Bus_Function => Step.Bus_Function,
                           Address      => Step.Address,
                           Length       => Step.Length,
                           Value        => Value);

                        exit when (Value and Step.Mask) /= 0;

                        if J = Step.Trys then
                           Success := False;
                           return;
                        end if;

                        RP.Device.Timer.Delay_Milliseconds (1);
                     end loop;

                  when Write_Register =>
                     Write_Register
                       (Bus_Function => Step.Bus_Function,
                        Address      => Step.Address,
                        Length       => Step.Length,
                        Value        => Step.Value);

                  when Write_Register_Variable =>
                     Write_Register
                       (Bus_Function => Step.Bus_Function,
                        Address      => Step.Address,
                        Length       => Step.Length,
                        Value        => Custom_Value);

                  when Upload_Firmware =>
                     Upload_Firmware
                       (Address =>
                         (if Step.Firmware = 1 then 0 else 16#7_FCFC#),
                        Data    =>
                         (if Step.Firmware = 1 then Firmware_1
                          else Firmware_2));

                  when Wait_Any_Event =>
                     for J in 1 .. 100 loop
                        exit when Has_Event;

                        if J = 100 then
                           Success := False;
                           return;
                        end if;

                        RP.Device.Timer.Delay_Milliseconds (1);
                     end loop;

                  when Clear_Error =>
                     Clear_Error;

                  when Sleep =>
                     RP.Device.Timer.Delay_Milliseconds (Step.Milliseconds);
               end case;

               Index := Index + 1;
            end;
         end loop;
      end Execute;

   end Executor;

   package body IOCTL is

      type Output_IOCTL_Command is record
         Prefix  : HAL.UInt8_Array (1 .. Write_Prefix_Length);
         SDPCM   : Generic_IO.SDPCM.SDPCM_Header;
         IOCTL   : IOCTL_Header;
         Data    : HAL.UInt8_Array (1 .. 1536);
      end record
        with Pack;

      TX_Command  : Output_IOCTL_Command;
      TX_Sequence : Interfaces.Unsigned_8 := 0;
      TX_Request  : Interfaces.Unsigned_16 := 0;

      ---------
      -- Get --
      ---------

      procedure Get
        (State   : in out Joining_State;
         Command : IOCTL.Command := 262;
         Name    : HAL.UInt8_Array;
         Data    : out HAL.UInt8_Array)
      is
         use type Interfaces.Unsigned_8;

         Out_Length : constant Interfaces.Unsigned_16 :=
           (Name'Length + Data'Length + 3) / 4 * 4;

         Length     : constant Interfaces.Unsigned_16 :=
           Interfaces.Unsigned_16 (Write_Prefix_Length) +
           (SDPCM.SDPCM_Header'Size + IOCTL_Header'Size) / 8 +
           Out_Length;

         Raw : HAL.UInt8_Array (1 .. Positive (Length))
           with Import, Address => TX_Command'Address;

      begin
         TX_Sequence := TX_Sequence + 1;
         TX_Request := Interfaces.Unsigned_16'Succ (TX_Request);

         TX_Command :=
           (Prefix  => Write_Prefix
              (Bus_Function => CYW4343X.WLAN,
               Address      => 0,
               Length       => Positive (Length) - Write_Prefix_Length),
            SDPCM   =>
              (Tag      => Make_Tag
                   (Length - Interfaces.Unsigned_16 (Write_Prefix_Length)),
               Sequence => TX_Sequence,
               Channel  => SDPCM.Control,
               Next_Len => 0,
               Hdr_Len  => SDPCM.SDPCM_Header'Size / 8,
               Flow     => 0,
               Credit   => 0,
               Reserved => 0),
            IOCTL   =>
              (Command    => Command,
               Out_Length => Out_Length,
               In_Length  => 0,
               Flags      => Interfaces.Unsigned_32 (TX_Request) * 2**16,
               Status     => 0),
            Data    => <>);

         TX_Command.Data (1 .. Name'Length) := Name;

         Write
           (Bus_Function => CYW4343X.WLAN,
            Address      => 0,
            Value        => Raw);

         for J in 1 .. 1000 loop
            declare
               use type SDPCM.SDPCM_Channel;
               Got  : SDPCM.Packet;
               To   : Natural;
            begin
               SDPCM.Event_Poll (State, Input, Got, To);

               if To = 0 then
                  RP.Device.Timer.Delay_Milliseconds (1);
               elsif Got.Channel = SDPCM.Control and then
                 Got.IOCTL_Header.Command = Command
               then
                  declare
                     From : constant Natural := Got.IOCTL_Offset;
                     Size : constant Natural := To - From + 1;
                  begin
                     if Size >= Data'Length then
                        Data := HAL.UInt8_Array
                          (Input (From .. From + Data'Length - 1));
                     else
                        raise Program_Error;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set
        (State   : in out Joining_State;
         Command : IOCTL.Command;
         Name    : HAL.UInt8_Array;
         Data    : HAL.UInt8_Array)
      is
         use type Interfaces.Unsigned_8;

         Out_Length : constant Interfaces.Unsigned_16 :=
           (Name'Length + Data'Length + 3) / 4 * 4;

         Length     : constant Interfaces.Unsigned_16 :=
           Interfaces.Unsigned_16 (Write_Prefix_Length) +
           (SDPCM.SDPCM_Header'Size + IOCTL_Header'Size) / 8 +
           Out_Length;

         Raw : HAL.UInt8_Array (1 .. Positive (Length))
           with Import, Address => TX_Command'Address;

      begin
         TX_Sequence := TX_Sequence + 1;
         TX_Request := Interfaces.Unsigned_16'Succ (TX_Request);

         TX_Command :=
           (Prefix => Write_Prefix
              (Bus_Function => CYW4343X.WLAN,
               Address      => 0,
               Length       => Positive (Length) - Write_Prefix_Length),
            SDPCM  =>
              (Tag      => Make_Tag
                   (Length - Interfaces.Unsigned_16 (Write_Prefix_Length)),
               Sequence => TX_Sequence,
               Channel  => SDPCM.Control,
               Next_Len => 0,
               Hdr_Len  => SDPCM.SDPCM_Header'Size / 8,
               Flow     => 0,
               Credit   => 0,
               Reserved => 0),
            IOCTL  =>
              (Command    => Command,
               Out_Length => Out_Length,
               In_Length  => 0,
               Flags      =>
                 Interfaces.Unsigned_32 (TX_Request) * 2**16 + 2,
               Status     => 0),
            Data   => <>);

         TX_Command.Data (1 .. Name'Length) := Name;

         TX_Command.Data (Name'Length + 1 .. Name'Length + Data'Length)
           := Data;

         TX_Command.Data
           (Name'Length + Data'Length + 1 .. Natural (Out_Length)) :=
             [others => 0];

         Write
           (Bus_Function => CYW4343X.WLAN,
            Address      => 0,
            Value        => Raw);

         for J in 1 .. 1000 loop
            declare
               use type SDPCM.SDPCM_Channel;
               Got : SDPCM.Packet;
               To  : Natural;
            begin
               SDPCM.Event_Poll (State, Input, Got, To);

               if To = 0 then
                  RP.Device.Timer.Delay_Milliseconds (1);
               elsif Got.Channel = SDPCM.Control then
                  exit when Got.IOCTL_Header.Command = Command;
               end if;
            end;
         end loop;
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set
        (State   : in out Joining_State;
         Command : IOCTL.Command;
         Name    : String;
         Data    : HAL.UInt8_Array)
      is
         Raw_Name : HAL.UInt8_Array (1 .. Name'Length + 1);
      begin
         if Name = "" then
            Set (State, Command, Raw_Name (1 .. 0), Data);
         else
            for J in Name'Range loop
               Raw_Name (J) := Character'Pos (Name (J));
            end loop;

            Raw_Name (Raw_Name'Last) := 0;

            Set (State, Command, Raw_Name, Data);
         end if;
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set
        (State   : in out Joining_State;
         Command : IOCTL.Command;
         Name    : String;
         Data    : Interfaces.Unsigned_32)
      is
         subtype Word is HAL.UInt8_Array (1 .. 4);

         function To_Bytes is new Ada.Unchecked_Conversion
           (Interfaces.Unsigned_32, Word);

         Raw : constant Word := To_Bytes (Data);
      begin
         Set (State, Command, Name, Raw);
      end Set;

   end IOCTL;

   ------------------
   -- Change_State --
   ------------------

   procedure Change_State (State : in out Joining_State) is
   begin
      case State.Kind is
         when Idle =>
            State :=
              (Kind      => Joining,
               Expire    => Timeout (10),
               Link_Up   => False,
               Link_Auth => False,
               Link_Fail => False);

            Restart_Join (State);
         when Joining =>
            if State.Link_Up and State.Link_Auth then
               State.Kind := Joined;
            elsif State.Link_Fail or else Is_Expired (State.Expire) then
               State.Kind := Failed;
               State.Expire := Timeout (10);
               Stop_Join (State);
            end if;
         when Joined =>
            if State.Link_Fail then
               State.Kind := Failed;
               State.Expire := Timeout (10);
               Stop_Join (State);
            end if;
         when Failed =>
            if Is_Expired (State.Expire) then
               State.Kind := Idle;
            end if;
      end case;
   end Change_State;

   --------------
   -- CLM_Load --
   --------------

   procedure CLM_Load
     (State : in out Joining_State;
      Data  : HAL.UInt8_Array)
   is
      MAX_LOAD_LEN : constant := 512;

      type CLM_Load_Request is record
         Req  : String (1 .. 8);
         Flag : Interfaces.Unsigned_16;
         Tipe : Interfaces.Unsigned_16;
         Len  : Interfaces.Unsigned_32;
         Crc  : Interfaces.Unsigned_32;
      end record;

      for CLM_Load_Request use record
         Req  at 0  range 0 .. 8 * 8 - 1;
         Flag at 8  range 0 .. 15;
         Tipe at 10 range 0 .. 15;
         Len  at 12 range 0 .. 31;
         Crc  at 16 range 0 .. 31;
      end record;

      NUL : constant Character := Character'Val (0);

      Request : CLM_Load_Request :=
         (Req  => ['c', 'l', 'm', 'l', 'o', 'a', 'd', others => NUL],
          Flag => 0,
          Tipe => 2,
          Len  => 0,
          Crc  => 0);

      Raw_Name : HAL.UInt8_Array (1 .. Request'Size / 8)
        with Import, Address => Request'Address;

      From : Positive := Data'First;
   begin
      while From <= Data'Last loop
         declare
            N : constant Positive :=
              Positive'Min (MAX_LOAD_LEN, (Data'Last - From + 1));

            Flag : constant Positive :=
                ((if From = Data'First then 2 else 0) +
                 (if From + N > Data'Last then 4 else 0) +
                 16#1000#);
         begin
            Request.Len := Interfaces.Unsigned_32 (N);
            Request.Flag := Interfaces.Unsigned_16 (Flag);

            IOCTL.Set
              (State   => State,
               Command => IOCTL.WLC_SET_VAR,
               Name    => Raw_Name,
               Data    => Data (From .. From + N - 1));

            From := From + N;
         end;
      end loop;
   end CLM_Load;

   ----------------
   -- Event_Poll --
   ----------------

   procedure Event_Poll (State : in out Joining_State) is
      Ignore  : SDPCM.Packet;
      To      : Natural;
   begin
      SDPCM.Event_Poll (State, Input, Ignore, To);
      Change_State (State);
   end Event_Poll;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response
     (Data   : out Input_Buffer;
      Last   : out Natural)
   is
      Length : constant Interfaces.Unsigned_32 := Available_Packet_Length;
   begin
      if Length = 0 then
         Last := 0;
      elsif Length <= Interfaces.Unsigned_32 (Data'Last) then
         Last := Positive (Length);

         Read
           (Bus_Function => CYW4343X.WLAN,
            Address      => 0,
            Value        => HAL.UInt8_Array (Data (1 .. Last)));
      else
         raise Program_Error;
      end if;
   end Get_Response;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (State    : in out Joining_State;
      Firmware : HAL.UInt8_Array;
      NVRAM    : HAL.UInt8_Array;
      CLM      : HAL.UInt8_Array;
      My_MAC   : out Ether_Addr;
      Success  : out Boolean)
   is
      use type Executor.Step_Array;

      package Reset_RAM is new Executor.Reset (Use_RAM => True);
      package Reset_ARM is new Executor.Reset (Use_RAM => False);

      List : constant Executor.Step_Array :=
        Executor.Step_Array'
        (1 =>
           (Kind         => Executor.Read_Register,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Length       => 1),
         --  Check Active Low Power (ALP) clock
         2 =>
           (Kind         => Executor.Write_Register,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Value        => 16#08#,
            Length       => 1),
         3 =>
           (Kind         => Executor.Read_Register_Until,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Mask         => 16#40#,
            Length       => 1,
            Trys         => 10),
         4 =>
           (Kind         => Executor.Write_Register,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Value        => 0,
            Length       => 1))
        &
            Reset_RAM.List
        &
        Executor.Step_Array'
        (14 =>
          (Kind         => Executor.Read_Register,
           Bus_Function => CYW4343X.Backplane,
           Address      => Backplane_Register.Chip_Clock_CSR,
           Length       => 1),
         --  Write 0x18004010 and 0x18004044
         15 =>
          (Kind         => Executor.Write_Register,
           Bus_Function => CYW4343X.Backplane,
           Address      => Backplane_Register.Win_Addr,
           Value        => 16#180000#,
           Length       => 3),
         16 =>
          (Kind         => Executor.Write_Register,
           Bus_Function => CYW4343X.Backplane,
           Address      => 16#04010#,
           Value        => 3,
           Length       => 4),
         17 =>
          (Kind         => Executor.Write_Register,
           Bus_Function => CYW4343X.Backplane,
           Address      => 16#04044#,
           Value        => 0,
           Length       => 4),
         18 =>
          (Kind         => Executor.Upload_Firmware,
           Firmware     => 1),
         19 =>
          (Kind         => Executor.Sleep,
           Milliseconds => 5),
         20 =>
          (Kind         => Executor.Upload_Firmware,
           Firmware     => 2),
         21 =>
           (Kind         => Executor.Write_Register_Variable,
            Bus_Function => CYW4343X.Backplane,
            Address      => 16#FFFC#,  --  NVRAM size register
            Length       => 4))
        &
        Reset_ARM.List
        &
        Executor.Step_Array'
        (31 =>
           (Kind         => Executor.Read_Register_Until,
            Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Mask         => 16#80#,
            Length       => 1,
            Trys         => 50),
         32 =>
           (Kind => Executor.Wait_Any_Event));

   begin
      Success := True;

      Executor.Execute
        (List,
         Success,
         Firmware_1   => Firmware,
         Firmware_2   => NVRAM,
         Custom_Value => Make_Tag (NVRAM'Length / 4));

      if Success then
         CLM_Load (State, CLM);
      end if;

      declare
         BAK_GPIOOUTEN_REG : constant := 16#8068#;
      begin
         Write_Register
           (Bus_Function => CYW4343X.Backplane,
            Address      => Backplane_Register.Win_Addr,
            Value        => 16#180000#,
            Length       => 3);

         Write_Register
           (Bus_Function => CYW4343X.Backplane,
            Address      => BAK_GPIOOUTEN_REG,
            Value        => 1,
            Length       => 4);

         Turn_LED (False);
      end;

      declare
         Name : constant String := "cur_etheraddr" & Character'Val (0);
         Raw  : HAL.UInt8_Array (Name'Range)
           with Import, Address => Name'Address;
      begin
         IOCTL.Get (State, Name => Raw, Data => My_MAC);
      end;

      Success := True;
   end Initialize;

   procedure Restart_Join (State : in out Joining_State) is
      Mode : constant CYW4343X.Security_Mode := Security_Mode;
   begin
      IOCTL.Set (State, IOCTL.WLC_UP, "", []);
      IOCTL.Set (State, IOCTL.WLC_SET_GMODE, "", 1);
      IOCTL.Set (State, IOCTL.WLC_SET_BAND, "", 0);
      IOCTL.Set (State, IOCTL.WLC_SET_VAR, "pm2_sleep_ret", 16#C8#);
      IOCTL.Set (State, IOCTL.WLC_SET_VAR, "bcn_li_bcn", 1);
      IOCTL.Set (State, IOCTL.WLC_SET_VAR, "bcn_li_dtim", 1);
      IOCTL.Set (State, IOCTL.WLC_SET_VAR, "assoc_listen", 16#0A#);
      IOCTL.Set (State, IOCTL.WLC_SET_INFRA, "", 1);
      IOCTL.Set (State, IOCTL.WLC_SET_AUTH, "", 0);

      case Mode is
         when WPA_TKIP | WPA2_AES =>
            IOCTL.Set
              (State,
               IOCTL.WLC_SET_WSEC,
               "",
               (if Mode = WPA2_AES then 6 else 2));

            IOCTL.Set
              (State,
               IOCTL.WLC_SET_VAR,
               "bsscfg:sup_wpa",
               [16#00#, 16#00#, 16#00#, 16#00#,
                16#01#, 16#00#, 16#00#, 16#00#]);

            IOCTL.Set
              (State,
               IOCTL.WLC_SET_VAR,
               "bsscfg:sup_wpa2_eapver",
               [16#00#, 16#00#, 16#00#, 16#00#,
                16#FF#, 16#FF#, 16#FF#, 16#FF#]);

            IOCTL.Set
              (State,
               IOCTL.WLC_SET_VAR,
               "bsscfg:sup_wpa_tmo",
               [16#00#, 16#00#, 16#00#, 16#00#,
                16#C4#, 16#09#, 16#00#, 16#00#]);

            RP.Device.Timer.Delay_Milliseconds (2);

            --  Set password
            declare
               Value : constant String := Password;
               Raw   : HAL.UInt8_Array (1 .. Value'Length + 4);
            begin
               Raw (1) := Value'Length;
               Raw (2) := 0;
               Raw (3) := 1;
               Raw (4) := 0;
               for J in Value'Range loop
                  Raw (4 + J) := Character'Pos (Value (J));
               end loop;
               IOCTL.Set (State, IOCTL.WLC_SET_WSEC_PMK, "", Raw);
            end;

            IOCTL.Set (State, IOCTL.WLC_SET_INFRA, "", 1);
            IOCTL.Set (State, IOCTL.WLC_SET_AUTH, "", 0);

            IOCTL.Set
              (State,
               IOCTL.WLC_SET_WPA_AUTH,
               "",
               (if Mode = WPA2_AES then 16#80# else 4));

            --  Set SSID
            declare
               Value : constant String := SSID;
               Raw   : HAL.UInt8_Array (1 .. Value'Length + 4);
            begin
               Raw (1) := Value'Length;
               Raw (2) := 0;
               Raw (3) := 0;
               Raw (4) := 0;
               for J in Value'Range loop
                  Raw (4 + J) := Character'Pos (Value (J));
               end loop;
               IOCTL.Set (State, IOCTL.WLC_SET_SSID, "", Raw);
            end;

         when None =>
            raise Program_Error;
      end case;
   end Restart_Join;

   ----------------
   -- Join_Start --
   ----------------

   procedure Start_Join
     (State   : in out Joining_State;
      Success : out Boolean;
      Country : Generic_IO.Country := XX_Country) is
   begin
      Success := True;

      Executor.Execute
        (Executor.Join_Start,
         Success,
         Firmware_1   => [],
         Firmware_2   => [],
         Custom_Value => 0);

      --  Set country
      IOCTL.Set
        (State, IOCTL.WLC_SET_VAR, "country", HAL.UInt8_Array (Country));
      --  Select antenna
      IOCTL.Set (State, IOCTL.WLC_SET_ANTDIV, "", [0, 0, 0, 0]);
      --  Data aggregation
      IOCTL.Set (State, IOCTL.WLC_SET_VAR, "bus:txglom", 0);
      IOCTL.Set (State, IOCTL.WLC_SET_VAR, "apsta", 1);
      IOCTL.Set (State, IOCTL.WLC_SET_VAR, "ampdu_ba_wsize", 8);
      IOCTL.Set (State, IOCTL.WLC_SET_VAR, "ampdu_mpdu", 4);
      IOCTL.Set (State, IOCTL.WLC_SET_VAR, "ampdu_rx_factor", 0);
      RP.Device.Timer.Delay_Milliseconds (150);

      --  Enable events for reporting the join process
      IOCTL.Set
        (State,
         IOCTL.WLC_SET_VAR,
         "bsscfg:event_msgs",
         Events.To_Raw_Event_Mask (Events.To_Mask (Events.Join_Events)));

      RP.Device.Timer.Delay_Milliseconds (50);

      --  Enable multicast
      declare
         List : constant HAL.UInt8_Array (1 .. 6 * 10) :=
           [1, 0, 0, 0,
            16#01#, 16#00#, 16#5E#, 16#00#, 16#00#, 16#FB#,
            others => 0];
      begin
         IOCTL.Set (State, IOCTL.WLC_SET_VAR, "mcast_list", List);

         RP.Device.Timer.Delay_Milliseconds (50);
      end;
   end Start_Join;

   ---------------
   -- Stop_Join --
   ---------------

   procedure Stop_Join (State : in out Joining_State) is
   begin
      IOCTL.Set (State, IOCTL.WLC_DOWN, "", []);
   end Stop_Join;

   --------------
   -- Turn_LED --
   --------------

   procedure Turn_LED (Value : Boolean) is
      BAK_GPIOOUT_REG   : constant := 16#8064#;
   begin
      Write_Register
        (Bus_Function => CYW4343X.Backplane,
         Address      => BAK_GPIOOUT_REG,
         Value        => (if Value then 1 else 0),
         Length       => 4);
   end Turn_LED;

   ---------------------
   -- Upload_Firmware --
   ---------------------

   procedure Upload_Firmware
     (Address : Interfaces.Unsigned_32;
      Data    : HAL.UInt8_Array)
   is
      Window_Size : constant := 16#8000#;
      Block_Size  : constant := 64;
      Offset      : Interfaces.Unsigned_32 := Address;
      From        : Natural := Data'First;
      Length      : Natural;
      Window      : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'Last;
      Piece       : HAL.UInt8_Array (1 .. Write_Prefix_Length + Block_Size);
   begin
      while From <= Data'Last loop
         Length := Natural'Min (Data'Last - From + 1, Block_Size);

         if Window /= Offset / Window_Size then
            Window := Offset / Window_Size;

            Write_Register
              (Bus_Function => CYW4343X.Backplane,
               Address      => Backplane_Register.Win_Addr,
               Value        => Window * Window_Size / 256,
               Length       => 3);
         end if;

         Piece (1 .. Write_Prefix_Length) := Write_Prefix
           (Bus_Function => CYW4343X.Backplane,
            Address      => Offset mod Window_Size,
            Length       => Length);

         Piece (Write_Prefix_Length + 1 .. Write_Prefix_Length + Length) :=
           Data (From .. From + Length - 1);

         Write
           (Bus_Function => CYW4343X.Backplane,
            Address      => Offset mod Window_Size,
            Value        => Piece (1 .. Write_Prefix_Length + Length));

         From := From + Length;
         Offset := Offset + Interfaces.Unsigned_32 (Length);
      end loop;
   end Upload_Firmware;

end CYW4343X.Generic_IO;
