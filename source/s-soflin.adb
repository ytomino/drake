with Ada.Unchecked_Conversion;
with System.Runtime_Context;
with System.Startup;
with System.Unwind;
package body System.Soft_Links is
   pragma Suppress (All_Checks);
   use type Ada.Exceptions.Exception_Occurrence_Access;

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
      return Cast (TLS.Current_Exception'Access);
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

   procedure Save_Library_Occurrence (
      E : Ada.Exceptions.Exception_Occurrence_Access)
   is
      function Cast is
         new Ada.Unchecked_Conversion (
            Ada.Exceptions.Exception_Occurrence_Access,
            Unwind.Exception_Occurrence_Access);
   begin
      if not Startup.Library_Exception_Set then
         Startup.Library_Exception_Set := True;
         if E /= null then
            Unwind.Save_Occurrence (Startup.Library_Exception.X, Cast (E).all);
         end if;
      end if;
   end Save_Library_Occurrence;

   function Zero return Integer is
   begin
      return 0;
   end Zero;

end System.Soft_Links;
