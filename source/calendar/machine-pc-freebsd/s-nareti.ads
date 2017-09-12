pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD (or Linux)
with System.Native_Time;
with C.time;
package System.Native_Real_Time is
   pragma Preelaborate;

   subtype Native_Time is C.time.struct_timespec;

   function To_Native_Time (T : Duration) return Native_Time
      renames System.Native_Time.To_timespec;
   function To_Duration (T : Native_Time) return Duration
      renames System.Native_Time.To_Duration;

   function Clock return Native_Time;

   --  same as Ada.Real_Time

   subtype Time is Duration;

   Tick : constant := 1.0 / 1_000_000;
      --  clock_gettime returns micro-seconds.

   --  for delay until

   procedure Simple_Delay_Until (T : Native_Time);

   type Delay_Until_Handler is access procedure (T : Native_Time);
   pragma Favor_Top_Level (Delay_Until_Handler);
   pragma Suppress (Access_Check, Delay_Until_Handler);

   --  equivalent to Timed_Delay (s-soflin.ads)
   Delay_Until_Hook : Delay_Until_Handler := Simple_Delay_Until'Access;
   pragma Suppress (Access_Check, Delay_Until_Hook); -- not null

   procedure Delay_Until (T : Native_Time);

end System.Native_Real_Time;
