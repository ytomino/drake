pragma License (Unrestricted);
--  implementation unit specialized for Darwin
with System.Native_Calendar;
package System.Synchronous_Objects.Abortable.Delays is
   pragma Preelaborate;

   procedure Delay_For (D : Duration);
   procedure Delay_Until_Calendar (T : Native_Calendar.Native_Time);

   procedure Register_Delays;
   procedure Unregister_Delays;

end System.Synchronous_Objects.Abortable.Delays;
