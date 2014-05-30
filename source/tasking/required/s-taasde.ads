pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Tasking.Async_Delays is

   --  required for select delay then abort by compiler (s-parame.ads)
   type Delay_Block is null record;

   --  required for select delay then abort by compiler (s-parame.ads)
   function Enqueue_Duration (
      T : Duration;
      D : not null access Delay_Block)
      return Boolean;

   --  required for select delay then abort by compiler (s-parame.ads)
   procedure Cancel_Async_Delay (D : not null access Delay_Block);
   function Timed_Out (D : not null access Delay_Block) return Boolean;

end System.Tasking.Async_Delays;
