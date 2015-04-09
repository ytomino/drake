pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD (or Linux)
with System.Native_Calendar;
with System.Native_Real_Time;
package System.Synchronous_Objects.Abortable.Delays is
   pragma Preelaborate;

   procedure Delay_For (D : Duration);
   procedure Delay_Until_Calendar (T : Native_Calendar.Native_Time);
   procedure Delay_Until_Real_Time (T : Native_Real_Time.Native_Time);

   procedure Register_Delays;
   procedure Unregister_Delays;

end System.Synchronous_Objects.Abortable.Delays;
