--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

with RP.Device;

package body CYW4343X.Generic_IO is

   package Backplane_Register is
      Chip_Clock_CSR : constant := 16#1000e#;  --  Chip clock ctrl
   end Backplane_Register;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Success : out Boolean) is
      use type Interfaces.Unsigned_32;

      Value : Interfaces.Unsigned_32;
   begin
      Success := True;

      --  Check Active Low Power (ALP) clock
      Write_Register
        (Bus_Function => Backplane,
         Address      => Backplane_Register.Chip_Clock_CSR,
         Value        => 16#08#,
         Length       => 1);

      for J in 1 .. 10 loop
         Read_Register
           (Bus_Function => Backplane,
            Address      => Backplane_Register.Chip_Clock_CSR,
            Length       => 1,
            Value        => Value);
         exit when (Value and 16#40#) /= 0;

         if J = 10 then
            Success := False;
         else
            RP.Device.Timer.Delay_Milliseconds (1);
         end if;
      end loop;
   end Initialize;

end CYW4343X.Generic_IO;
