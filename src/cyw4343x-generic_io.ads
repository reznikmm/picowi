--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Interfaces;
with HAL;

generic
   --  Bus interface
   with procedure Read_Register
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : out Interfaces.Unsigned_32);

   with procedure Write_Register
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : Interfaces.Unsigned_32);

   Write_Prefix_Length : Natural;

   with function Write_Prefix
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive) return HAL.UInt8_Array;

   with procedure Write
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Value        : HAL.UInt8_Array);

   with procedure Read
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Value        : out HAL.UInt8_Array);

   with function Has_Event return Boolean;

   with function Available_Packet_Length return Interfaces.Unsigned_32;

   with procedure Clear_Error;

   --  Join parameters:
   with function SSID     return String;
   with function Password return String;
   with function Security_Mode return CYW4343X.Security_Mode;

   --  Timeout interface
   type Time is private;
   with function Timeout (Second : Natural) return Time;
   with function Is_Expired (Timeout : Time) return Boolean;
package CYW4343X.Generic_IO is

   type Joining_State is private;

   subtype Ether_Addr is HAL.UInt8_Array (1 .. 6);

   procedure Initialize
     (State    : in out Joining_State;
      Firmware : HAL.UInt8_Array;
      NVRAM    : HAL.UInt8_Array;
      CLM      : HAL.UInt8_Array;
      My_MAC   : out Ether_Addr;
      Success  : out Boolean);

   type Country is private;

   XX_Country : constant Country;
   --  country code of ‘XX’, which is a common set of world-wide
   --  characteristics.

   procedure Start_Join
     (State   : in out Joining_State;
      Success : out Boolean;
      Country : Generic_IO.Country := XX_Country);

   procedure Event_Poll (State : in out Joining_State);

   function Is_Joined (State : Joining_State) return Boolean;

   procedure Turn_LED (Value : Boolean);

   procedure Send (Data : HAL.UInt8_Array);

private
   type Country is new HAL.UInt8_Array (1 .. 20);

   XX_Country : constant Country :=
     (16#58#, 16#58#, 16#00#, 16#00#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#58#, 16#58#, others => 16#00#);
   --  "XX\x00\x00\xFF\xFF\xFF\xFFXX"

   type Joining_State_Kind is (Idle, Joining, Joined, Failed);

   type Joining_State is record
      Kind      : Joining_State_Kind := Idle;
      Expire    : Time;
      Link_Up   : Boolean := False;
      Link_Auth : Boolean := False;
      Link_Fail : Boolean := False;
   end record;

   function Is_Joined (State : Joining_State) return Boolean is
      (State.Kind = Joined);

end CYW4343X.Generic_IO;
