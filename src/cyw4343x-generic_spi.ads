--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with Interfaces;

with HAL;

generic
   with procedure Chip_Select (On : Boolean);
   with procedure Read (Data : out HAL.UInt8_Array);
   with procedure Write (Data : HAL.UInt8_Array);
package CYW4343X.Generic_SPI is

   procedure Detect_Chip (Success : out Boolean);

   procedure Switch_Endian (Success : out Boolean);

   procedure Read_Register
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : out Interfaces.Unsigned_32);

   procedure Write_Register
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive;
      Value        : Interfaces.Unsigned_32);

   procedure Read
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Value        : out HAL.UInt8_Array)
     with Pre => Bus_Function /= Backplane;

   subtype Output_Prefix is HAL.UInt8_Array (1 .. 4);

   function Write_Prefix
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Length       : Positive) return Output_Prefix;

   procedure Write
     (Bus_Function : CYW4343X.Bus_Function;
      Address      : Interfaces.Unsigned_32;
      Value        : HAL.UInt8_Array);
   --  Write Value to bus/address. Value should have Write_Prefix
   --  at the begiinning.

   function Has_Event return Boolean;

   function Available_Packet_Length return Interfaces.Unsigned_32;

   procedure Clear_Error;

end CYW4343X.Generic_SPI;
