pragma License (Unrestricted);
--  Ada 2012
with Ada.Finalization;
private with System.Tasking.Inside;
package Ada.Synchronous_Task_Control is
   pragma Preelaborate;

   type Suspension_Object is limited private;

   procedure Set_True (S : in out Suspension_Object);
   procedure Set_False (S : in out Suspension_Object);
   function Current_State (S : Suspension_Object) return Boolean;
   procedure Suspend_Until_True (S : in out Suspension_Object);

private

   type Suspension_Object is new Finalization.Limited_Controlled with record
      Object : System.Tasking.Inside.Event;
   end record;

   overriding procedure Initialize (Object : in out Suspension_Object);
   overriding procedure Finalize (Object : in out Suspension_Object);

end Ada.Synchronous_Task_Control;
