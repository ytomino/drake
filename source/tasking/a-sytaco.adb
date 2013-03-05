with System.Tasking.Synchronous_Objects.Abortable;
with System.Tasking.Tasks;
package body Ada.Synchronous_Task_Control is
   pragma Suppress (All_Checks);

   procedure Set_True (S : in out Suspension_Object) is
   begin
      System.Tasking.Synchronous_Objects.Set (S.Object);
   end Set_True;

   procedure Set_False (S : in out Suspension_Object) is
   begin
      System.Tasking.Synchronous_Objects.Reset (S.Object);
   end Set_False;

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      return System.Tasking.Synchronous_Objects.Get (S.Object);
   end Current_State;

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      Aborted : Boolean;
   begin
      System.Tasking.Tasks.Enable_Abort;
      System.Tasking.Synchronous_Objects.Abortable.Wait (
         S.Object,
         Aborted => Aborted);
      System.Tasking.Tasks.Disable_Abort (Aborted);
   end Suspend_Until_True;

   overriding procedure Initialize (Object : in out Suspension_Object) is
   begin
      System.Tasking.Synchronous_Objects.Initialize (Object.Object);
   end Initialize;

   overriding procedure Finalize (Object : in out Suspension_Object) is
   begin
      System.Tasking.Synchronous_Objects.Finalize (Object.Object);
   end Finalize;

end Ada.Synchronous_Task_Control;
