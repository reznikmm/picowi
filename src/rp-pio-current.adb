function RP.PIO.Current
  (This : PIO_Device;
   SM   : PIO_SM) return HAL.UInt5 is
begin
   return This.Periph.SM (SM).ADDR.SM0_ADDR;
end RP.PIO.Current;
