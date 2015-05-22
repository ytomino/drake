pragma License (Unrestricted);
--  optional runtime unit
with System;
package nosig is
   pragma Preelaborate;

   procedure Install_Exception_Handler (SEH : System.Address) is null
      with Export, -- for weak linking
         Convention => Ada,
         External_Name => "__drake_install_exception_handler";

   procedure Install_Task_Exception_Handler (
      SEH : System.Address;
      Signal_Stack : System.Address) is null
      with Export,
         Convention => Ada,
         External_Name => "__drake_install_task_exception_handler";

   procedure Reinstall_Exception_Handler is null
      with Export,
         Convention => Ada,
         External_Name => "__drake_reinstall_exception_handler";

   --  Win64 SEH only
   function New_Machine_Occurrence_From_SEH (
      Exception_Record : System.Address)
      return System.Address is (System.Null_Address)
      with Export,
         Convention => Ada,
         External_Name => "__drake_new_machine_occurrence_from_seh";

end nosig;
