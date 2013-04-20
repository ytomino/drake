function System.Tasking.Async_Delays.Enqueue_RT (
   T : Ada.Real_Time.Time;
   D : not null access Delay_Block)
   return Boolean is
begin
   raise Program_Error; -- unimplemented
   return Enqueue_RT (T, D);
end System.Tasking.Async_Delays.Enqueue_RT;
