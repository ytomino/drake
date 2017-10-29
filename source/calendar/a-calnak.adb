package body Ada.Calendar.Naked is

   function To_Native_Time (T : Time)
      return System.Native_Calendar.Native_Time is
   begin
      return System.Native_Calendar.To_Native_Time (Duration (T));
   end To_Native_Time;

   function To_Time (T : System.Native_Calendar.Native_Time) return Time is
   begin
      return Time (System.Native_Calendar.To_Time (T));
   end To_Time;

end Ada.Calendar.Naked;
