pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Real_Time;
function System.Tasking.Async_Delays.Enqueue_RT (
   T : Ada.Real_Time.Time;
   D : not null access Delay_Block)
   return Boolean;
