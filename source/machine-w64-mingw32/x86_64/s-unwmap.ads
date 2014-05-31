pragma License (Unrestricted);
--  runtime unit
with System.Unwind.Handling;
with C.winnt;
package System.Unwind.Mapping is
   pragma Preelaborate;

   --  register signal handler (init.c/seh_init.c)
   procedure Install_Exception_Handler (SEH : Address) is null;

   procedure Reinstall_Exception_Handler is null;

   --  signal alt stack
   type Signal_Stack_Type is private;
   procedure Set_Signal_Stack (S : access Signal_Stack_Type) is null;

   --  equivalent to __gnat_map_SEH (seh_init.c)
   --    and Create_Machine_Occurrence_From_Signal_Handler (a-except-2005.adb)
   function New_Machine_Occurrence_From_SEH (
      Exception_Record : C.winnt.struct_EXCEPTION_RECORD_ptr)
      return Handling.GNAT_GCC_Exception_Access;

private

   type Signal_Stack_Type is null record;
   pragma Suppress_Initialization (Signal_Stack_Type);

   --  for weak linking,
   --  this symbol will be linked other symbols are used
   Install_Exception_Handler_Ref : constant not null access procedure (
      SEH : Address) :=
      Install_Exception_Handler'Access;
   pragma Export (Ada, Install_Exception_Handler_Ref,
      "__drake_ref_install_exception_handler");

end System.Unwind.Mapping;
