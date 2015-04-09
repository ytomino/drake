pragma License (Unrestricted);
--  implementation unit specialized for Darwin
with C.stdint;
with C.mach.mach_time;
package System.Native_Real_Time is

   subtype Native_Time is C.stdint.uint64_t;

   function To_Duration (D : Native_Time) return Duration;

   pragma Pure_Function (To_Duration);

   function Clock return Native_Time
      renames C.mach.mach_time.mach_absolute_time;

   --  same as Ada.Real_Time

   subtype Time is Duration;

   Tick : constant := 1.0 / 1000_000_000;
   --  mach_absolute_time returns nano-seconds

   --  for delay until

   procedure Delay_Until (T : Native_Time); -- no hook for Darwin

   generic
      type Ada_Time is new Duration;
   procedure Generic_Delay_Until (T : Ada_Time);
   pragma Inline (Generic_Delay_Until);

end System.Native_Real_Time;
