pragma License (Unrestricted);
--  overridable runtime unit specialized for Windows (x86_64)
with System.Unwind.Representation;
with C.winnt;
package System.Unwind.Mapping is
   pragma Preelaborate;

   --  signal alt stack
   type Signal_Stack_Type is private;

   --  register signal handler (init.c/seh_init.c)
   procedure Install_Exception_Handler (SEH : Address) is null
      with Export, -- for weak linking
         Convention => Ada,
         External_Name => "__drake_install_exception_handler";

   procedure Install_Task_Exception_Handler (
      SEH : Address;
      Signal_Stack : not null access Signal_Stack_Type) is null
      with Export,
         Convention => Ada,
         External_Name => "__drake_install_task_exception_handler";

   procedure Reinstall_Exception_Handler is null
      with Export,
         Convention => Ada,
         External_Name => "__drake_reinstall_exception_handler";

   --  equivalent to __gnat_map_SEH (seh_init.c)
   --    and Create_Machine_Occurrence_From_Signal_Handler (a-except-2005.adb)
   function New_Machine_Occurrence_From_SEH (
      Exception_Record : C.winnt.struct_EXCEPTION_RECORD_ptr)
      return Representation.Machine_Occurrence_Access
      with Export,
         Convention => Ada,
         External_Name => "__drake_new_machine_occurrence_from_seh";

   pragma No_Inline (New_Machine_Occurrence_From_SEH);

private

   type Signal_Stack_Type is null record;
   pragma Suppress_Initialization (Signal_Stack_Type);

end System.Unwind.Mapping;
