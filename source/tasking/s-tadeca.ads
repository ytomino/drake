pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Calendar;
function System.Tasking.Async_Delays.Enqueue_Calendar (
   T : Ada.Calendar.Time;
   D : not null access Delay_Block)
   return Boolean;
