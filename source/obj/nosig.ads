pragma License (Unrestricted);
--  optional runtime unit
with System;
package nosig is
   pragma Preelaborate;

   procedure Install_Exception_Handler (SEH : System.Address) is null;
   pragma Export (Ada, Install_Exception_Handler,
      "__drake_install_exception_handler");

   procedure Install_Task_Exception_Handler (
      SEH : System.Address;
      Signal_Stack : System.Address) is null;
   pragma Export (Ada, Install_Task_Exception_Handler,
      "__drake_install_task_exception_handler");

   procedure Reinstall_Exception_Handler is null;
   pragma Export (Ada, Reinstall_Exception_Handler,
      "__drake_reinstall_exception_handler");

   --  Win64 SEH only
   function New_Machine_Occurrence_From_SEH (
      Exception_Record : System.Address)
      return System.Address is (System.Null_Address);
   pragma Export (Ada, New_Machine_Occurrence_From_SEH,
      "__drake_new_machine_occurrence_from_seh");

private

   --  for weak linking,
   --  this symbol will be linked other symbols are used
   Install_Exception_Handler_Ref : constant not null access procedure (
      SEH : System.Address) :=
      Install_Exception_Handler'Access;
   pragma Export (Ada, Install_Exception_Handler_Ref,
      "__drake_ref_install_exception_handler");

end nosig;
