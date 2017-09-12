pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Exceptions;
with System.Synchronous_Control;
package System.Soft_Links is
   pragma Preelaborate;

   function Do_Get_Current_Excep
      return Ada.Exceptions.Exception_Occurrence_Access;

   type Get_Current_Excep_Handler is
      access function return Ada.Exceptions.Exception_Occurrence_Access;
   pragma Favor_Top_Level (Get_Current_Excep_Handler);
   pragma Suppress (Access_Check, Get_Current_Excep_Handler);

   --  required for elaboration of packages by compiler (s-soflin.ads)
   --  the result would passed to Save_Occurrence or Save_Library_Occurrence.
   Get_Current_Excep : constant Get_Current_Excep_Handler :=
      Do_Get_Current_Excep'Access;
   pragma Suppress (Access_Check, Get_Current_Excep); -- not null

   --  required for entry call by compiler (s-soflin.ads)
   --  the result would passed to Exceptional_Complete_Rendezvous,
   --    Exceptional_Complete_Entry_Body
   --    or Exceptional_Complete_Single_Entry_Body.
   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id;
   pragma Inline (Get_GNAT_Exception);

   --  required for library-level controlled object by compiler (s-soflin.ads)
   procedure Save_Library_Occurrence (
      X : Ada.Exceptions.Exception_Occurrence_Access)
      with Import,
         Convention => Ada,
         External_Name => "system__soft_links__save_library_occurrence";

   --  no-operation
   function Zero return Integer; -- always return 0
   procedure Nop
      renames Synchronous_Control.Nop;

   type Current_Master_Handler is access function return Integer;
   pragma Favor_Top_Level (Current_Master_Handler);
   pragma Suppress (Access_Check, Current_Master_Handler);

   type Enter_Master_Handler is access procedure;
   pragma Favor_Top_Level (Enter_Master_Handler);
   pragma Suppress (Access_Check, Enter_Master_Handler);

   type Complete_Master_Handler is access procedure;
   pragma Favor_Top_Level (Complete_Master_Handler);
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

   --  not required for gcc (s-soflin.ads)
--  procedure Update_Exception_NT (X : EO := Current_Target_Exception);

end System.Soft_Links;
