with Ada.Exception_Identification.From_Here;
with System.Debug;
with System.Native_Time;
with System.Tasks;
with C.pthread;
with C.time;
package body System.Synchronous_Objects.Abortable.Delays is
   use Ada.Exception_Identification.From_Here;
   use type C.signed_int;

   procedure Wait_Real_Time (
      Object : in out Condition_Variable;
      Mutex : in out Synchronous_Objects.Mutex;
      Timeout : Native_Real_Time.Native_Time;
      Notified : out Boolean;
      Aborted : out Boolean);

   procedure Wait_Real_Time (
      Object : in out Condition_Variable;
      Mutex : in out Synchronous_Objects.Mutex;
      Timeout : Native_Real_Time.Native_Time;
      Notified : out Boolean;
      Aborted : out Boolean) is
   begin
      Notified := False;
      Aborted := Tasks.Is_Aborted;
      if not Aborted then
         declare
            Timeout_T : constant Duration := Native_Time.To_Duration (Timeout);
            N : Duration := Native_Time.To_Duration (Native_Real_Time.Clock);
         begin
            loop
               N := N + Abort_Checking_Span;
               exit when N >= Timeout_T;
               Wait (
                  Object,
                  Mutex,
                  Timeout => Native_Time.To_timespec (N),
                  Notified => Notified);
               Aborted := Tasks.Is_Aborted;
               if Notified or else Aborted then
                  return;
               end if;
            end loop;
            Wait (Object, Mutex, Timeout, Notified);
            Aborted := Tasks.Is_Aborted;
         end;
      end if;
   end Wait_Real_Time;

   --  implementation

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

   procedure Delay_Until_Real_Time (T : Native_Real_Time.Native_Time) is
      Aborted : Boolean;
      Error : Boolean;
   begin
      Tasks.Enable_Abort;
      declare
         M : Mutex;
         CV : Condition_Variable;
         condattr : aliased C.pthread.pthread_condattr_t;
         Notified : Boolean;
      begin
         Initialize (M);
         --  initialize a condition variable for CLOCK_MONOTONIC
         Error := C.pthread.pthread_condattr_init (condattr'Access) < 0;
         if not Error then
            Error := C.pthread.pthread_condattr_setclock (
               condattr'Access,
               C.time.CLOCK_MONOTONIC) < 0;
            if not Error then
               Error := C.pthread.pthread_cond_init (
                  CV.Handle'Access,
                  condattr'Access) < 0;
               if not Error then
                  --  waiting
                  Enter (M);
                  Wait_Real_Time (
                     CV,
                     M,
                     Timeout => T,
                     Notified => Notified,
                     Aborted => Aborted);
                  Leave (M);
                  Finalize (CV);
               end if;
            end if;
            declare
               R : C.signed_int;
            begin
               R := C.pthread.pthread_condattr_destroy (condattr'Access);
               pragma Check (Debug,
                  Check =>
                     R = 0
                     or else Debug.Runtime_Error (
                        "pthread_condattr_destroy failed"));
            end;
         end if;
         Finalize (M);
      end;
      if Error then
         Aborted := Tasks.Is_Aborted;
      end if;
      Tasks.Disable_Abort (Aborted);
      if Error then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Delay_Until_Real_Time;

   procedure Register_Delays is
   begin
      Native_Time.Delay_For_Hook := Delay_For'Access;
      Native_Calendar.Delay_Until_Hook := Delay_Until_Calendar'Access;
      Native_Real_Time.Delay_Until_Hook := Delay_Until_Real_Time'Access;
   end Register_Delays;

   procedure Unregister_Delays is
   begin
      Native_Time.Delay_For_Hook := Native_Time.Simple_Delay_For'Access;
      Native_Calendar.Delay_Until_Hook :=
         Native_Calendar.Simple_Delay_Until'Access;
      Native_Real_Time.Delay_Until_Hook :=
         Native_Real_Time.Simple_Delay_Until'Access;
   end Unregister_Delays;

end System.Synchronous_Objects.Abortable.Delays;
