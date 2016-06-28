pragma License (Unrestricted);
--  implementation unit specialized for Darwin
with C.stdint;
with C.mach.mach_time;
package System.Native_Real_Time is
   pragma Preelaborate;

   subtype Native_Time is C.stdint.uint64_t;

   function To_Native_Time (T : Duration) return Native_Time;
   function To_Duration (T : Native_Time) return Duration;

   pragma Pure_Function (To_Native_Time);
   pragma Pure_Function (To_Duration);

   function Clock return Native_Time
      renames C.mach.mach_time.mach_absolute_time;

   --  same as Ada.Real_Time

   subtype Time is Duration;

   Tick : constant := 1.0 / 1_000_000_000;
      --  mach_absolute_time returns nano-seconds.

   --  for delay until

   procedure Delay_Until (T : Native_Time); -- no hook for Darwin

end System.Native_Real_Time;
