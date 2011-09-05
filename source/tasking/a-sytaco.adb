package body Ada.Synchronous_Task_Control is
   pragma Suppress (All_Checks);

   procedure Set_True (S : in out Suspension_Object) is
   begin
      System.Tasking.Inside.Set (S.Object);
   end Set_True;

   procedure Set_False (S : in out Suspension_Object) is
   begin
      System.Tasking.Inside.Reset (S.Object);
   end Set_False;

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      return System.Tasking.Inside.Get (S.Object);
   end Current_State;

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      Aborted : Boolean;
   begin
      System.Tasking.Inside.Enable_Abort;
      System.Tasking.Inside.Wait (S.Object, Aborted => Aborted);
      System.Tasking.Inside.Disable_Abort (Aborted);
   end Suspend_Until_True;

   overriding procedure Initialize (Object : in out Suspension_Object) is
   begin
      System.Tasking.Inside.Initialize (Object.Object);
   end Initialize;

   overriding procedure Finalize (Object : in out Suspension_Object) is
   begin
      System.Tasking.Inside.Finalize (Object.Object);
   end Finalize;

end Ada.Synchronous_Task_Control;
