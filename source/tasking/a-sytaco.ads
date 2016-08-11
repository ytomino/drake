pragma License (Unrestricted);
private with Ada.Finalization;
private with System.Storage_Barriers;
private with System.Synchronous_Objects;
package Ada.Synchronous_Task_Control is
   pragma Preelaborate;

   type Suspension_Object is limited private;

   procedure Set_True (S : in out Suspension_Object);
   procedure Set_False (S : in out Suspension_Object);
   function Current_State (S : Suspension_Object) return Boolean;
   --  modified
   procedure Suspend_Until_True (
      S : in out Suspension_Object;
      Multi : Boolean := False); -- additional

   pragma Inline (Current_State);

private

   type Suspension_Object is
      limited new Finalization.Limited_Controlled with
   record
      Object : System.Synchronous_Objects.Event;
      Waiting : aliased System.Storage_Barriers.Flag; -- for CXDA002
   end record;

   overriding procedure Initialize (Object : in out Suspension_Object);
   overriding procedure Finalize (Object : in out Suspension_Object);

end Ada.Synchronous_Task_Control;
