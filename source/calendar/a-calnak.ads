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

end Ada.Calendar.Naked;
