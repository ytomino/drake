package body System.Standard_Library is
   pragma Suppress (All_Checks);

   --  weak reference for System.Unwind.Tracebacks (ELF only ?)
   procedure Install_Exception_Handler (SEH : Address);
   pragma Import (Ada, Install_Exception_Handler,
      "__drake_install_exception_handler");
   pragma Weak_External (Install_Exception_Handler);

   procedure Initialize (SEH : Address) is
   begin
      if Install_Exception_Handler'Address /= Null_Address then
         Install_Exception_Handler (SEH);
         Handler_Installed := 1;
      end if;
   end Initialize;

end System.Standard_Library;
