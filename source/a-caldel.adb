package body Ada.Calendar.Delays is

   procedure Delay_Until (T : Time) is
   begin
      System.Native_Time.Delay_Until (
         System.Native_Time.To_Native_Time (Duration (T)));
   end Delay_Until;

end Ada.Calendar.Delays;
