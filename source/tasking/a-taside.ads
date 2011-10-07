pragma License (Unrestricted);
private with System.Tasking.Inside;
package Ada.Task_Identification is
   pragma Preelaborate;

   type Task_Id is private;
   pragma Preelaborable_Initialization (Task_Id);

   Null_Task_Id : constant Task_Id;

--  function "=" (Left, Right : Task_Id) return Boolean;
   --  predefined "=" is right

   function Image (T : Task_Id) return String;
   function Current_Task return Task_Id;
   function Environment_Task return Task_Id;
   procedure Abort_Task (T : Task_Id);

   function Is_Terminated (T : Task_Id) return Boolean;
   function Is_Callable (T : Task_Id) return Boolean;
   function Activation_Is_Complete (T : Task_Id) return Boolean;

private

   type Task_Id is new System.Tasking.Inside.Task_Id;

   Null_Task_Id : constant Task_Id := null;

   function Current_Task return Task_Id
      renames Current_Task_Id; -- inherited

   function Environment_Task return Task_Id
      renames Main_Task_Id; -- inherited

   procedure Abort_Task (T : Task_Id)
      renames Send_Abort; -- inherited

   function Is_Terminated (T : Task_Id) return Boolean
      renames Terminated; -- inherited

   function Is_Callable (T : Task_Id) return Boolean
      renames Callable; -- inherited

   function Activation_Is_Complete (T : Task_Id) return Boolean
      renames Activated; -- inherited

end Ada.Task_Identification;
