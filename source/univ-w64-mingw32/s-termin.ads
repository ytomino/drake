pragma License (Unrestricted);
--  runtime unit
package System.Termination is
   pragma Preelaborate;

   --  write to standard error output
   procedure Error_Put (S : String);
   procedure Error_New_Line;

   --  force to abort
   procedure Force_Abort;
   pragma No_Return (Force_Abort);

   --  register exit handler
   type Exit_Handler is access procedure;
   procedure Register_Exit (Handler : not null Exit_Handler);

   --  register signal handler (init.c/seh_init.c)
   procedure Install_Exception_Handler (SEH : Address);

   procedure Reinstall_Exception_Handler;

   --  signal alt stack
   type Signal_Stack_Type is private;
   procedure Set_Signal_Stack (S : access Signal_Stack_Type) is null;

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

end System.Termination;
