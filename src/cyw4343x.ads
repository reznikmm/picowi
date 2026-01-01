--  SPDX-FileCopyrightText: 2025 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
----------------------------------------------------------------

package CYW4343X is
   pragma Pure;

   type Bus_Function is (Bus, Backplane, WLAN);

   type Security_Mode is (None, WPA_TKIP, WPA2_AES);

end CYW4343X;
