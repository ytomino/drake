function System.Tasking.Async_Delays.Enqueue_Calendar (
   T : Ada.Calendar.Time;
   D : not null access Delay_Block)
   return Boolean is
begin
   raise Program_Error; -- unimplemented
   return Enqueue_Calendar (T, D);
end System.Tasking.Async_Delays.Enqueue_Calendar;
