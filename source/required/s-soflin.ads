pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Exceptions;
with System.Synchronous_Control;
package System.Soft_Links is
   pragma Preelaborate;

   --  required for getting "Ada.Exceptions".Exception_Occurrence
   --    by compiler (s-soflin.ads)
   function Do_Get_Current_Excep
      return Ada.Exceptions.Exception_Occurrence_Access;
   Get_Current_Excep : constant access function
      return Ada.Exceptions.Exception_Occurrence_Access :=
      Do_Get_Current_Excep'Access;
   pragma Suppress (Access_Check, Get_Current_Excep); -- not null

   --  required for entry of task by compiler (s-soflin.ads)
   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id;
   pragma Inline (Get_GNAT_Exception);

   --  required for library-level controlled object by compiler (s-soflin.ads)
   procedure Save_Library_Occurrence (
      E : Ada.Exceptions.Exception_Occurrence_Access);

   --  no-operation
   function Zero return Integer; -- always return 0
   procedure Nop
      renames Synchronous_Control.Nop;

   type Current_Master_Handler is access function return Integer;
   pragma Suppress (Access_Check, Current_Master_Handler);
   type Enter_Master_Handler is access procedure;
   pragma Suppress (Access_Check, Enter_Master_Handler);
   type Complete_Master_Handler is access procedure;
   pragma Suppress (Access_Check, Complete_Master_Handler);

   --  required for controlled types and task by compiler (s-soflin.ads)
   Current_Master : Current_Master_Handler := Zero'Access;
   pragma Suppress (Access_Check, Current_Master); -- not null

   --  required for task by compiler (s-soflin.ads)
   Enter_Master : Enter_Master_Handler := Nop'Access;
   pragma Suppress (Access_Check, Enter_Master); -- not null
   Complete_Master : Complete_Master_Handler := Nop'Access;
   pragma Suppress (Access_Check, Complete_Master); -- not null

   --  required for many times by compiler (s-soflin.ads)
   Abort_Defer : Synchronous_Control.Lock_Abort_Handler
      renames Synchronous_Control.Lock_Abort_Hook;
   pragma Suppress (Access_Check, Abort_Defer); -- not null

   --  required for exception handler by compiler (s-soflin.ads)
   Abort_Undefer : Synchronous_Control.Unlock_Abort_Handler
      renames Synchronous_Control.Unlock_Abort_Hook;
   pragma Suppress (Access_Check, Abort_Undefer); -- not null

   --  required for limited interface by compiler (s-soflin.ads)
   type Dummy_Communication_Block is record
      Comp_1 : Address;
      Comp_2 : Boolean;
      Comp_3 : Boolean;
   end record;
   pragma Suppress_Initialization (Dummy_Communication_Block);

end System.Soft_Links;
