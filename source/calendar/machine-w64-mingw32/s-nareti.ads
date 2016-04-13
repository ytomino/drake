pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C.winnt;
package System.Native_Real_Time is

   subtype Native_Time is C.winnt.LARGE_INTEGER;

   function To_Native_Time (T : Duration) return Native_Time;
   function To_Duration (T : Native_Time) return Duration;

   pragma Pure_Function (To_Native_Time);
   pragma Pure_Function (To_Duration);

   function Clock return Native_Time;

   --  same as Ada.Real_Time

   subtype Time is Duration;

   Tick : constant := 1.0 / 1_000_000_000;
      --  QueryPerformanceCounter returns nano-seconds.

   --  for delay until

   procedure Delay_Until (T : Native_Time); -- no hook for Windows

end System.Native_Real_Time;
