with System.Tasking.Inside;
with System.Tasking.Synchronous_Objects.Abortable;
package body Ada.Synchronous_Task_Control.EDF is
   pragma Suppress (All_Checks);

   procedure Suspend_Until_True_And_Set_Deadline (
      S : in out Suspension_Object;
      TS : Real_Time.Time_Span)
   is
      Dummy : Boolean;
   begin
      Suspend_Until_True_And_Set_Deadline (S, TS, Dummy);
   end Suspend_Until_True_And_Set_Deadline;

   procedure Suspend_Until_True_And_Set_Deadline (
      S : in out Suspension_Object;
      TS : Real_Time.Time_Span;
      State : out Boolean)
   is
      Aborted : Boolean;
   begin
      System.Tasking.Inside.Enable_Abort;
      System.Tasking.Synchronous_Objects.Abortable.Wait (
         S.Object,
         Real_Time.To_Duration (TS),
         State,
         Aborted => Aborted);
      System.Tasking.Inside.Disable_Abort (Aborted);
   end Suspend_Until_True_And_Set_Deadline;

end Ada.Synchronous_Task_Control.EDF;
