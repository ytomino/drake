pragma License (Unrestricted);
pragma Compiler_Unit;
--  implementation package
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
   pragma Export (Ada, Install_Exception_Handler,
      "__drake_install_exception_handler");
   pragma Weak_External (Install_Exception_Handler);

end System.Termination;
