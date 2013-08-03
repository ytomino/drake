package body System.Tasking.Async_Delays is

   function Enqueue_Duration (
      T : Duration;
      D : not null access Delay_Block)
      return Boolean is
   begin
      raise Program_Error; -- unimplemented
      return Enqueue_Duration (T, D);
   end Enqueue_Duration;

   procedure Cancel_Async_Delay (D : not null access Delay_Block) is
   begin
      raise Program_Error; -- unimplemented
   end Cancel_Async_Delay;

   function Timed_Out (D : not null access Delay_Block) return Boolean is
   begin
      raise Program_Error; -- unimplemented
      return Timed_Out (D);
   end Timed_Out;

end System.Tasking.Async_Delays;
