pragma License (Unrestricted);
--  implementation package required by compiler
package System.Task_Info is
   pragma Preelaborate;

   --  required for task and pragma Task_Info by compiler (s-parame.ads)
   type Scope_Type is (Process_Scope, System_Scope, Default_Scope);
   pragma Discard_Names (Scope_Type);
   type Task_Info_Type is new Scope_Type;
   pragma Discard_Names (Task_Info_Type);
   Unspecified_Task_Info : constant Task_Info_Type := Default_Scope;

end System.Task_Info;
