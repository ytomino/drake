pragma License (Unrestricted);
--  overridable runtime unit specialized for POSIX (Darwin, FreeBSD, or Linux)
private with C.signal;
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

   procedure Reinstall_Exception_Handler is null;
   pragma Export (Ada, Reinstall_Exception_Handler,
      "__drake_reinstall_exception_handler");

private

   Signal_Stack_Storage_Count : constant :=
      C.size_t'Max (C.signal.MINSIGSTKSZ, 16#1000#); -- 4096

   type Signal_Stack_Type is array (
      1 ..
      Signal_Stack_Storage_Count) of aliased C.char;
   pragma Suppress_Initialization (Signal_Stack_Type);
   for Signal_Stack_Type'Size use
      Signal_Stack_Storage_Count * Standard'Storage_Unit;

end System.Unwind.Mapping;
