private with System.Native_Real_Time;
package body Ada.Real_Time.Delays is

   procedure Delay_Until (T : Time) is
   begin
      System.Native_Real_Time.Delay_Until (
         System.Native_Real_Time.To_Native_Time (Duration (T)));
   end Delay_Until;

end Ada.Real_Time.Delays;
