package body System.Startup is
   pragma Suppress (All_Checks);

   --  weak reference for System.Unwind.Mapping
   procedure Install_Exception_Handler (SEH : Address)
      with Import, -- weak linking
         Convention => Ada,
         External_Name => "__drake_install_exception_handler";

   pragma Weak_External (Install_Exception_Handler);

   --  implementation

   procedure Initialize (SEH : Address) is
   begin
      if Install_Exception_Handler'Address /= Null_Address then
         Install_Exception_Handler (SEH);
      end if;
   end Initialize;

end System.Startup;
