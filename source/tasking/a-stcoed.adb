package body Ada.Synchronous_Task_Control.EDF is

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
      State : out Boolean) is
   begin
      System.Tasking.Inside.Wait (S.Object, Real_Time.To_Duration (TS), State);
   end Suspend_Until_True_And_Set_Deadline;

end Ada.Synchronous_Task_Control.EDF;
