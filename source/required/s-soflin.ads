pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Exceptions;
with System.Synchronous_Control;
package System.Soft_Links is
   pragma Preelaborate;

   --  required for elaboration of packages by compiler (s-soflin.ads)
   --  the result would passed to Save_Occurrence or Save_Library_Occurrence.
   function Do_Get_Current_Excep
      return Ada.Exceptions.Exception_Occurrence_Access;
   Get_Current_Excep : constant
      not null access
         function return Ada.Exceptions.Exception_Occurrence_Access :=
      Do_Get_Current_Excep'Access;

   --  required for entry call by compiler (s-soflin.ads)
   --  the result would passed to Exceptional_Complete_Rendezvous,
   --    Exceptional_Complete_Entry_Body
   --    or Exceptional_Complete_Single_Entry_Body.
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
   type Enter_Master_Handler is access procedure;
   type Complete_Master_Handler is access procedure;

   --  required for controlled types and task by compiler (s-soflin.ads)
   Current_Master : not null Current_Master_Handler := Zero'Access;

   --  required for task by compiler (s-soflin.ads)
   Enter_Master : not null Enter_Master_Handler := Nop'Access;
   Complete_Master : not null Complete_Master_Handler := Nop'Access;

   --  required for many times by compiler (s-soflin.ads)
   Abort_Defer : Synchronous_Control.Lock_Abort_Handler
      renames Synchronous_Control.Lock_Abort_Hook;

   --  required for exception handler by compiler (s-soflin.ads)
   Abort_Undefer : Synchronous_Control.Unlock_Abort_Handler
      renames Synchronous_Control.Unlock_Abort_Hook;

   --  required for limited interface by compiler (s-soflin.ads)
   type Dummy_Communication_Block is record
      Comp_1 : Address;
      Comp_2 : Boolean;
      Comp_3 : Boolean;
   end record;
   pragma Suppress_Initialization (Dummy_Communication_Block);

end System.Soft_Links;
