pragma License (Unrestricted);
--  overridable runtime unit specialized for POSIX (Darwin, FreeBSD, or Linux)
private with C.signal;
package System.Unwind.Mapping is
   pragma Preelaborate;

   --  signal alt stack
   type Signal_Stack_Type is private;
   procedure Set_Signal_Stack (S : access Signal_Stack_Type);
   pragma Export (Ada, Set_Signal_Stack, "__drake_set_signal_stack");

   --  register signal handler (init.c/seh_init.c)
   procedure Install_Exception_Handler (SEH : Address);
   pragma Export (Ada, Install_Exception_Handler,
      "__drake_install_exception_handler");

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

   --  for weak linking,
   --  this symbol will be linked other symbols are used
   Install_Exception_Handler_Ref : constant not null access procedure (
      SEH : Address) :=
      Install_Exception_Handler'Access;
   pragma Export (Ada, Install_Exception_Handler_Ref,
      "__drake_ref_install_exception_handler");

end System.Unwind.Mapping;
