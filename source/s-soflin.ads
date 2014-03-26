pragma License (Unrestricted);
--  runtime unit required by compiler
with Ada.Exceptions;
package System.Soft_Links is
   pragma Preelaborate;

   --  required for getting "Ada.Exceptions".Exception_Occurrence
   --    by compiler (s-soflin.ads)
   function Do_Get_Current_Excep
      return Ada.Exceptions.Exception_Occurrence_Access;
   Get_Current_Excep : constant not null access function
      return Ada.Exceptions.Exception_Occurrence_Access :=
      Do_Get_Current_Excep'Access;
   pragma Suppress (Access_Check, Get_Current_Excep);

   --  required for entry of task by compiler (s-soflin.ads)
   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id;
   pragma Inline (Get_GNAT_Exception);

   --  required for library-level controlled object by compiler (s-soflin.ads)
   procedure Save_Library_Occurrence (
      E : Ada.Exceptions.Exception_Occurrence_Access);

   --  no-operation
   function Zero return Integer; -- always return 0
   procedure Nop is null;

   --  required for controlled types and task by compiler (s-soflin.ads)
   Current_Master : not null access function return Integer := Zero'Access;
   pragma Suppress (Access_Check, Current_Master);

   --  required for task by compiler (s-soflin.ads)
   Enter_Master : not null access procedure :=  Nop'Access;
   pragma Suppress (Access_Check, Enter_Master);
   Complete_Master : not null access procedure :=  Nop'Access;
   pragma Suppress (Access_Check, Complete_Master);

   --  required for many times by compiler (s-soflin.ads)
   Abort_Defer : not null access procedure := Nop'Access;
   pragma Suppress (Access_Check, Abort_Defer);

   --  implementation of System.Standard_Library.Abort_Undefer_Direct;
   procedure Abort_Undefer_Direct;
   pragma Export (Ada, Abort_Undefer_Direct,
      "system__standard_library__abort_undefer_direct");

   --  required for exception handler by compiler (s-soflin.ads)
   Abort_Undefer : not null access procedure := Nop'Access;
   pragma Suppress (Access_Check, Abort_Undefer);

   --  required for limited interface by compiler (s-soflin.ads)
   type Dummy_Communication_Block is record
      Comp_1 : Address;
      Comp_2 : Boolean;
      Comp_3 : Boolean;
   end record;
   pragma Suppress_Initialization (Dummy_Communication_Block);

end System.Soft_Links;
