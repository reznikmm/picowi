--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System.Storage_Elements;

with RP.Device;

package body CYW4343X.Generic_IO is

   package Backplane_Register is
      Win_Addr : constant := 16#1000a#;  --  Window addr
      Chip_Clock_CSR : constant := 16#1000e#;  --  Chip clock ctrl
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

                  when Upload_Firmware | Wait_Any_Event | Sleep =>
                     null;
               end case;

            when Upload_Firmware =>
               Firmware : Positive;

            when Wait_Any_Event =>
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

         List : Step_Array :=
           (1 =>
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
               Milliseconds => 1));
      end Reset;

   end Executor;

   procedure Upload_Firmware
     (Address : Interfaces.Unsigned_32;
      Data    : HAL.UInt8_Array);

   procedure CLM_Load (Data : HAL.UInt8_Array);

   procedure Get_Response
     (Data   : out HAL.UInt8_Array;
      Last   : out Natural);

   package IOCTL is
      type Command is new Interfaces.Unsigned_32;

      procedure Set
        (Command : IOCTL.Command;
         Name    : HAL.UInt8_Array;
         Data    : HAL.UInt8_Array);

      procedure Get
        (Command : IOCTL.Command := 262;
         Name    : HAL.UInt8_Array;
         Data    : out HAL.UInt8_Array);

   end IOCTL;

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

                  when Sleep =>
                     RP.Device.Timer.Delay_Milliseconds (Step.Milliseconds);
               end case;

               Index := Index + 1;
            end;
         end loop;
      end Execute;

   end Executor;

   package body IOCTL is

      type BDC_Header is record
         Flags    : Interfaces.Unsigned_8;
         Priority : Interfaces.Unsigned_8;
         Flags2   : Interfaces.Unsigned_8;
         Offset   : Interfaces.Unsigned_8;
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
      pragma Unreferenced (Event);
      Data    : constant SDPCM_Channel := 2;

      type SDPCM_Header is record
         Tag      : Frame_Tag;
         Sequence : Interfaces.Unsigned_8;
         Channel  : SDPCM_Channel;
         Next_Len : Interfaces.Unsigned_8;
         Hdr_Len  : Interfaces.Unsigned_8;  --  SDPCM header plus any padding
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

      subtype IOCTL_Header_Raw is HAL.UInt8_Array (1 .. IOCTL_Header'Size / 8);

      function To_IOCTL_Header is new Ada.Unchecked_Conversion
        (IOCTL_Header_Raw, IOCTL_Header);

      type IOCTL_Command is record
         SDPCM : SDPCM_Header;
         IOCTL : IOCTL_Header;
         Data  : HAL.UInt8_Array (1 .. 1536);
      end record;

      for IOCTL_Command use record
         SDPCM at 0  range 0 .. 12 * 8 - 1;
         IOCTL at 12 range 0 .. 16 * 8 - 1;
         Data  at 28 range 0 .. 1536 * 8 - 1;
      end record;

      procedure Decode_Input
        (Last    : Positive;
         Command : IOCTL.Command;
         Success : out Boolean);

      procedure Decode_Input
        (Last    : Positive;
         Command : IOCTL.Command;
         Data    : out HAL.UInt8_Array;
         Success : out Boolean);

      Input       : HAL.UInt8_Array (1 .. 1500)
        with Alignment => 4;

      TX_Command  : IOCTL_Command;
      TX_Sequence : Interfaces.Unsigned_8 := 0;
      TX_Request  : Interfaces.Unsigned_16 := 0;

      ------------
      -- Decode --
      ------------

      procedure Decode_Input
        (Last    : Positive;
         Command : IOCTL.Command;
         Success : out Boolean)
      is
         SDPCM : SDPCM_Header
           with Import, Address => Input'Address;
      begin
         Success := False;

         if Last < (SDPCM_Header'Size + IOCTL_Header'Size) / 8 then
            return;
         elsif Is_Valid (SDPCM.Tag)
           and then SDPCM.Channel <= IOCTL.Data
         then
            declare
               use System.Storage_Elements;

               BDC : BDC_Header
                 with Import,
                 Address => Input'Address + Storage_Offset (SDPCM.Hdr_Len);

               Hdr_Len : constant Positive := Positive (SDPCM.Hdr_Len);

               Header : constant IOCTL_Header := To_IOCTL_Header
                 (Input (Hdr_Len + 1 .. Hdr_Len + IOCTL_Header'Size / 8));

            begin
               if SDPCM.Channel = Control and then
                 Command = Header.Command
               then
                  Success := True;
               end if;
            end;
         end if;
      end Decode_Input;

      ------------------
      -- Decode_Input --
      ------------------

      procedure Decode_Input
        (Last    : Positive;
         Command : IOCTL.Command;
         Data    : out HAL.UInt8_Array;
         Success : out Boolean)
      is
         SDPCM : SDPCM_Header
           with Import, Address => Input'Address;
      begin
         Success := False;

         if Last < (SDPCM_Header'Size + IOCTL_Header'Size) / 8 then
            return;
         elsif Is_Valid (SDPCM.Tag)
           and then SDPCM.Channel <= IOCTL.Data
         then
            declare
               use System.Storage_Elements;

               BDC : BDC_Header
                 with Import,
                 Address => Input'Address + Storage_Offset (SDPCM.Hdr_Len);

               Skip : constant Positive := Positive (SDPCM.Hdr_Len);

               Header : constant IOCTL_Header := To_IOCTL_Header
                 (Input (Skip + 1 .. Skip + IOCTL_Header'Size / 8));

               Header_Length : constant Positive :=
                 Skip + IOCTL_Header'Size / 8;

               In_Length     : constant Integer :=
                 Last - Header_Length;
            begin
               if SDPCM.Channel = Control and then
                 Command = Header.Command
               then
                  declare
                     Size : constant Natural :=
                       Natural'Min (In_Length, Data'Length);
                  begin
                     Data (1 .. Size) :=
                       Input (Header_Length + 1 .. Header_Length + Size);

                     Success := True;
                  end;
               end if;
            end;
         end if;
      end Decode_Input;

      ---------
      -- Get --
      ---------

      procedure Get
        (Command : IOCTL.Command := 262;
         Name    : HAL.UInt8_Array;
         Data    : out HAL.UInt8_Array)
      is
         use type Interfaces.Unsigned_8;

         Out_Length : constant Interfaces.Unsigned_16 :=
           (Name'Length + Data'Length + 3) / 4 * 4;

         Length     : constant Interfaces.Unsigned_16 :=
           (SDPCM_Header'Size + IOCTL_Header'Size) / 8 + Out_Length;

         Raw : HAL.UInt8_Array (1 .. Positive (Length))
           with Import, Address => TX_Command'Address;

      begin
         TX_Sequence := TX_Sequence + 1;
         TX_Request := Interfaces.Unsigned_16'Succ (TX_Request);

         TX_Command :=
           (SDPCM =>
              (Tag      => Make_Tag (Length),
               Sequence => TX_Sequence,
               Channel  => Control,
               Next_Len => 0,
               Hdr_Len  => SDPCM_Header'Size / 8,
               Flow     => 0,
               Credit   => 0,
               Reserved => 0),
            IOCTL =>
              (Command    => Command,
               Out_Length => Out_Length,
               In_Length  => 0,
               Flags      => Interfaces.Unsigned_32 (TX_Request) * 2**16,
               Status     => 0),
            Data  => <>);

         TX_Command.Data (1 .. Name'Length) := Name;

         Write
           (Bus_Function => CYW4343X.WLAN,
            Address      => 0,
            Value        => Raw);

         for J in 1 .. 1000 loop
            declare
               Last : Natural;
               Ok   : Boolean;
            begin
               Get_Response (Input, Last);

               if Last = 0 then
                  RP.Device.Timer.Delay_Milliseconds (1);
               else
                  Decode_Input (Last, Command, Data, Ok);
                  exit when Ok;
               end if;
            end;
         end loop;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set
        (Command : IOCTL.Command;
         Name    : HAL.UInt8_Array;
         Data    : HAL.UInt8_Array)
      is
         use type Interfaces.Unsigned_8;

         Out_Length : constant Interfaces.Unsigned_16 :=
           (Name'Length + Data'Length + 3) / 4 * 4;

         Length     : constant Interfaces.Unsigned_16 :=
           (SDPCM_Header'Size + IOCTL_Header'Size) / 8 + Out_Length;

         Raw : HAL.UInt8_Array (1 .. Positive (Length))
           with Import, Address => TX_Command'Address;

      begin
         TX_Sequence := TX_Sequence + 1;
         TX_Request := Interfaces.Unsigned_16'Succ (TX_Request);

         TX_Command :=
           (SDPCM =>
              (Tag      => Make_Tag (Length),
               Sequence => TX_Sequence,
               Channel  => Control,
               Next_Len => 0,
               Hdr_Len  => SDPCM_Header'Size / 8,
               Flow     => 0,
               Credit   => 0,
               Reserved => 0),
            IOCTL =>
              (Command    => Command,
               Out_Length => Out_Length,
               In_Length  => 0,
               Flags      => Interfaces.Unsigned_32 (TX_Request) * 2**16 + 2,
               Status     => 0),
            Data  => <>);

         TX_Command.Data (1 .. Name'Length) := Name;

         TX_Command.Data (Name'Length + 1 .. Name'Length + Data'Length) :=
           Data;

         TX_Command.Data
           (Name'Length + Data'Length + 1 .. Natural (Out_Length)) :=
             (others => 0);

         Write
           (Bus_Function => CYW4343X.WLAN,
            Address      => 0,
            Value        => Raw);

         for J in 1 .. 1000 loop
            declare
               Last : Natural;
               Ok   : Boolean;
            begin
               Get_Response (Input, Last);

               if Last = 0 then
                  RP.Device.Timer.Delay_Milliseconds (1);
               else
                  Decode_Input (Last, Command, Ok);
                  exit when Ok;
               end if;
            end;
         end loop;
      end Set;

   end IOCTL;

   --------------
   -- CLM_Load --
   --------------

   procedure CLM_Load (Data : HAL.UInt8_Array) is
      MAX_LOAD_LEN : constant := 512;
      WLC_SET_VAR : constant := 263;

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
         (Req  => ('c', 'l', 'm', 'l', 'o', 'a', 'd', others => NUL),
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
              (Command => WLC_SET_VAR,
               Name    => Raw_Name,
               Data    => Data (From .. From + N - 1));

            From := From + N;
         end;
      end loop;

   end CLM_Load;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response
     (Data   : out HAL.UInt8_Array;
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
            Value        => Data (1 .. Last));
      else
         raise Program_Error;
      end if;
   end Get_Response;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Firmware : HAL.UInt8_Array;
      NVRAM    : HAL.UInt8_Array;
      CLM      : HAL.UInt8_Array;
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
         CLM_Load (CLM);
      end if;

      declare
         BAK_GPIOOUT_REG   : constant := 16#8064#;
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

         Write_Register
           (Bus_Function => CYW4343X.Backplane,
            Address      => BAK_GPIOOUT_REG,
            Value        => 1,
            Length       => 4);
      end;

      declare
         MAC  : HAL.UInt8_Array (1 .. 6);
         Name : constant String := "cur_etheraddr" & Character'Val (0);
         Raw  : HAL.UInt8_Array (Name'Range)
           with Import, Address => Name'Address;
      begin
         IOCTL.Get (Name => Raw, Data => MAC);
      end;

      Success := True;
   end Initialize;

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

         Write
           (Bus_Function => CYW4343X.Backplane,
            Address      => Offset mod Window_Size,
            Value        => Data (From .. From + Length - 1));

         From := From + Length;
         Offset := Offset + Interfaces.Unsigned_32 (Length);
      end loop;
   end Upload_Firmware;

end CYW4343X.Generic_IO;
