with Ada.Unchecked_Conversion;
with System.Runtime_Context;
with System.Unwind.Occurrences;
package body System.Soft_Links is
   pragma Suppress (All_Checks);

   --  implementation

   function Do_Get_Current_Excep
      return Ada.Exceptions.Exception_Occurrence_Access
   is
      function Cast is
         new Ada.Unchecked_Conversion (
            Unwind.Exception_Occurrence_Access,
            Ada.Exceptions.Exception_Occurrence_Access);
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
   begin
      return Cast (Unwind.Occurrences.Get_Current_Occurrence (TLS));
   end Do_Get_Current_Excep;

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id is
      function Cast is
         new Ada.Unchecked_Conversion (
            Ada.Exceptions.Exception_Occurrence_Access,
            Unwind.Exception_Occurrence_Access);
      function Cast is
         new Ada.Unchecked_Conversion (
            Unwind.Exception_Data_Access,
            Ada.Exceptions.Exception_Id);
   begin
      return Cast (Cast (Get_Current_Excep.all).Id);
   end Get_GNAT_Exception;

end System.Soft_Links;
