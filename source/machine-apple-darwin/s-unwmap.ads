pragma License (Unrestricted);
--  runtime unit
private with C.signal;
package System.Unwind.Mapping is
   pragma Preelaborate;

   --  register signal handler (init.c/seh_init.c)
   procedure Install_Exception_Handler (SEH : Address);

   procedure Reinstall_Exception_Handler is null;

   --  signal alt stack
   type Signal_Stack_Type is private;
   procedure Set_Signal_Stack (S : access Signal_Stack_Type);

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
