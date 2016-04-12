with System.Synchronous_Objects.Abortable;
with System.Tasks;
package body Ada.Synchronous_Task_Control is

   --  implementation

   procedure Set_True (S : in out Suspension_Object) is
   begin
      System.Synchronous_Objects.Set (S.Object);
   end Set_True;

   procedure Set_False (S : in out Suspension_Object) is
   begin
      System.Synchronous_Objects.Reset (S.Object);
   end Set_False;

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      return System.Synchronous_Objects.Get (S.Object);
   end Current_State;

   procedure Suspend_Until_True (
      S : in out Suspension_Object;
      Multi : Boolean := False)
   is
      Aborted : Boolean;
   begin
      if System.Storage_Barriers.atomic_test_and_set (S.Waiting'Access) then
         if not Multi then
            raise Program_Error; -- CXDA002, the waiter is limited to one
         end if;
      end if;
      System.Tasks.Enable_Abort;
      System.Synchronous_Objects.Abortable.Wait (
         S.Object,
         Aborted => Aborted);
      System.Storage_Barriers.atomic_clear (S.Waiting'Access);
      System.Tasks.Disable_Abort (Aborted);
   end Suspend_Until_True;

   overriding procedure Initialize (Object : in out Suspension_Object) is
   begin
      System.Storage_Barriers.atomic_clear (Object.Waiting'Access);
      System.Synchronous_Objects.Initialize (Object.Object);
   end Initialize;

   overriding procedure Finalize (Object : in out Suspension_Object) is
   begin
      System.Synchronous_Objects.Finalize (Object.Object);
   end Finalize;

end Ada.Synchronous_Task_Control;
