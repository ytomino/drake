private with System.Native_Calendar;
package body Ada.Calendar.Delays is

   procedure Delay_Until (T : Time) is
   begin
      System.Native_Calendar.Delay_Until (
         System.Native_Calendar.To_Native_Time (Duration (T)));
   end Delay_Until;

   function To_Duration (T : Time) return Duration is
   begin
      return Duration (T);
   end To_Duration;

end Ada.Calendar.Delays;
