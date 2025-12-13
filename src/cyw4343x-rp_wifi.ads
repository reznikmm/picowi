--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with HAL;

package CYW4343X.RP_WiFi is

   --  pragma Preelaborate;

   procedure Configure_GPIO (Power_On : Boolean);
   procedure Power_On;

   procedure Write_SPI (Data : HAL.UInt8_Array);
   procedure Read_SPI (Data : out HAL.UInt8_Array);

   procedure CS_Clear;
   procedure CS_Set;

end CYW4343X.RP_WiFi;
