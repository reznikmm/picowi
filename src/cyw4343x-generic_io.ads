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

   procedure Initialize
     (Firmware : HAL.UInt8_Array;
      NVRAM    : HAL.UInt8_Array;
      CLM      : HAL.UInt8_Array;
      Success  : out Boolean);

   type Country is private;

   XX_Country : constant Country;
   --  country code of ‘XX’, which is a common set of world-wide
   --  characteristics.

   procedure Start_Join
     (Success : out Boolean;
      Country : Generic_IO.Country := XX_Country);

   type Joining_State is private;

   procedure Event_Poll (State : in out Joining_State);

private
   type Country is new HAL.UInt8_Array (1 .. 20);

   XX_Country : constant Country :=
     (16#58#, 16#58#, 16#00#, 16#00#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#58#, 16#58#, others => 16#00#);
   --  "XX\x00\x00\xFF\xFF\xFF\xFFXX"

   type Joining_State_Kind is (Idle, Joining, Joined, Failed);

   type Joining_State is record
      Kind   : Joining_State_Kind := Idle;
      Expire : Time;
   end record;

end CYW4343X.Generic_IO;
