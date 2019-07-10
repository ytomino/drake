pragma License (Unrestricted);
--  implementation unit
with System.Native_Calendar;
package Ada.Calendar.Naked is

   function To_Native_Time (T : Time)
      return System.Native_Calendar.Native_Time;
   function To_Time (T : System.Native_Calendar.Native_Time) return Time;

   pragma Pure_Function (To_Native_Time);
   pragma Pure_Function (To_Time);
   pragma Inline (To_Native_Time);
   pragma Inline (To_Time);

   function Seconds_From_2150 (T : Time) return Duration;
   pragma Pure_Function (Seconds_From_2150);
   pragma Inline (Seconds_From_2150);

   --  Note: "2150-01-01 00:00:00" is Time'(0.0), unless the leap seconds are
   --    enabled by the operating system.
   --    The origin is still "1901-01-01".

   --  for delay until

   procedure Delay_Until (T : Time);

end Ada.Calendar.Naked;
