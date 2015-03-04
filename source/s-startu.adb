package body System.Startup is
   pragma Suppress (All_Checks);

   --  weak reference for System.Unwind.Mapping
   Install_Exception_Handler : access procedure (SEH : Address);
   pragma Import (Ada, Install_Exception_Handler,
      "__drake_ref_install_exception_handler");
   pragma Weak_External (Install_Exception_Handler);

   --  implementation

   procedure Initialize (SEH : Address) is
   begin
      if Install_Exception_Handler'Address /= Null_Address then
         Install_Exception_Handler (SEH);
         Handler_Installed := 1;
      end if;
   end Initialize;

end System.Startup;
