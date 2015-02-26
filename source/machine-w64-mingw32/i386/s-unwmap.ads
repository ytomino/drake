pragma License (Unrestricted);
--  overridable runtime unit specialized for Windows (i386)
package System.Unwind.Mapping is
   pragma Preelaborate;

   --  signal alt stack
   type Signal_Stack_Type is private;
   procedure Set_Signal_Stack (S : access Signal_Stack_Type) is null;
   pragma Export (Ada, Set_Signal_Stack, "__drake_set_signal_stack");

   --  register signal handler (init.c/seh_init.c)
   procedure Install_Exception_Handler (SEH : Address);
   pragma Export (Ada, Install_Exception_Handler,
      "__drake_install_exception_handler");

   procedure Reinstall_Exception_Handler;
   pragma Export (Ada, Reinstall_Exception_Handler,
      "__drake_reinstall_exception_handler");

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
