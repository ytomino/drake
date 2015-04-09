pragma License (Unrestricted);
--  overridable runtime unit specialized for Windows (i386)
package System.Unwind.Mapping is
   pragma Preelaborate;

   --  signal alt stack
   type Signal_Stack_Type is private;

   --  register signal handler (init.c/seh_init.c)
   procedure Install_Exception_Handler (SEH : Address)
      with Export, -- for weak linking
         Convention => Ada,
         External_Name => "__drake_install_exception_handler";

   pragma No_Inline (Install_Exception_Handler);

   procedure Install_Task_Exception_Handler (
      SEH : Address;
      Signal_Stack : not null access Signal_Stack_Type);
   pragma Export (Ada, Install_Task_Exception_Handler,
      "__drake_install_task_exception_handler");
   pragma No_Inline (Install_Task_Exception_Handler);

   procedure Reinstall_Exception_Handler;
   pragma Export (Ada, Reinstall_Exception_Handler,
      "__drake_reinstall_exception_handler");
   pragma No_Inline (Reinstall_Exception_Handler);

private

   type Signal_Stack_Type is null record;
   pragma Suppress_Initialization (Signal_Stack_Type);

end System.Unwind.Mapping;
