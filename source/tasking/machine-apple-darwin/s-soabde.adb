with System.Native_Time;
with System.Tasks;
package body System.Synchronous_Objects.Abortable.Delays is

   procedure Delay_For (D : Duration) is
      Aborted : Boolean;
   begin
      Tasks.Enable_Abort;
      declare
         M : Mutex;
         C : Condition_Variable;
         Notified : Boolean;
      begin
         Initialize (M);
         Initialize (C);
         Enter (M);
         Wait (
            C,
            M,
            Timeout => Duration'Max (D, 0.0),
            Notified => Notified,
            Aborted => Aborted);
         Leave (M);
         Finalize (C);
         Finalize (M);
      end;
      Tasks.Disable_Abort (Aborted);
   end Delay_For;

   procedure Delay_Until_Calendar (T : Native_Calendar.Native_Time) is
      Aborted : Boolean;
   begin
      Tasks.Enable_Abort;
      declare
         M : Mutex;
         C : Condition_Variable;
         Notified : Boolean;
      begin
         Initialize (M);
         Initialize (C);
         Enter (M);
         Wait (
            C,
            M,
            Timeout => T,
            Notified => Notified,
            Aborted => Aborted);
         Leave (M);
         Finalize (C);
         Finalize (M);
      end;
      Tasks.Disable_Abort (Aborted);
   end Delay_Until_Calendar;

   procedure Register_Delays is
   begin
      Native_Time.Delay_For_Hook := Delay_For'Access;
      Native_Calendar.Delay_Until_Hook := Delay_Until_Calendar'Access;
   end Register_Delays;

   procedure Unregister_Delays is
   begin
      Native_Time.Delay_For_Hook := Native_Time.Simple_Delay_For'Access;
      Native_Calendar.Delay_Until_Hook :=
         Native_Calendar.Simple_Delay_Until'Access;
   end Unregister_Delays;

end System.Synchronous_Objects.Abortable.Delays;
