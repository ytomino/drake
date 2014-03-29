with System.Synchronous_Objects.Abortable;
with System.Tasks;
package body Ada.Synchronous_Task_Control is
   pragma Suppress (All_Checks);

   function sync_bool_compare_and_swap (
      A1 : not null access Flag;
      A2 : Flag;
      A3 : Flag)
      return Boolean;
   pragma Import (Intrinsic, sync_bool_compare_and_swap,
      "__sync_bool_compare_and_swap_1");

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
      if not sync_bool_compare_and_swap (S.Waiting'Access, 0, 1) then
         if not Multi then
            raise Program_Error; -- CXDA002, the waiter is limited to one
         end if;
      end if;
      System.Tasks.Enable_Abort;
      System.Synchronous_Objects.Abortable.Wait (
         S.Object,
         Aborted => Aborted);
      S.Waiting := 0;
      System.Tasks.Disable_Abort (Aborted);
   end Suspend_Until_True;

   overriding procedure Initialize (Object : in out Suspension_Object) is
   begin
      Object.Waiting := 0;
      System.Synchronous_Objects.Initialize (Object.Object);
   end Initialize;

   overriding procedure Finalize (Object : in out Suspension_Object) is
   begin
      System.Synchronous_Objects.Finalize (Object.Object);
   end Finalize;

end Ada.Synchronous_Task_Control;
