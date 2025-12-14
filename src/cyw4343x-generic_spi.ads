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
      Value        : out Interfaces.Unsigned_32);

end CYW4343X.Generic_SPI;
