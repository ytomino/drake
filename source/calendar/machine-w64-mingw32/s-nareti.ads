pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C.winnt;
package System.Native_Real_Time is

   subtype Native_Time is C.winnt.LARGE_INTEGER;

   function To_Duration (D : Native_Time) return Duration;
   pragma Pure_Function (To_Duration);

   function Clock return Native_Time;

   --  same as Ada.Real_Time

   subtype Time is Duration;

   Tick : constant := 1.0 / 1000_000_000;
   --  QueryPerformanceCounter returns nano-seconds

   --  for delay until

   procedure Delay_Until (T : Native_Time); -- no hook for Windows

   generic
      type Ada_Time is new Duration;
   procedure Generic_Delay_Until (T : Ada_Time);
   pragma Inline (Generic_Delay_Until);

end System.Native_Real_Time;
