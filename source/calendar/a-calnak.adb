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

   function Seconds_From_2150 (T : Time) return Duration is
   begin
      return Duration (T);
   end Seconds_From_2150;

   procedure Delay_Until (T : Time) is
   begin
      System.Native_Calendar.Delay_Until (To_Native_Time (T));
   end Delay_Until;

end Ada.Calendar.Naked;
