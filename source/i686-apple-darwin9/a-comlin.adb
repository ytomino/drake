with System.Standard_Library;
with System.Zero_Terminated_Strings;
package body Ada.Command_Line is
   pragma Suppress (All_Checks);

   function Argv (Index : Natural) return String;
   function Argv (Index : Natural) return String is
      type String_Array is array (Natural) of System.Address;
      argv : String_Array;
      for argv'Address use System.Standard_Library.gnat_argv;
   begin
      return System.Zero_Terminated_Strings.Value (argv (Index));
   end Argv;

   --  implementation

   function Argument (Number : Positive) return String is
   begin
      if Number >= System.Standard_Library.gnat_argc then
         raise Constraint_Error;
      else
         return Argv (Number);
      end if;
   end Argument;

   function Argument_Count return Natural is
   begin
      return System.Standard_Library.gnat_argc - 1;
   end Argument_Count;

   function Command_Name return String is
   begin
      return Argv (0);
   end Command_Name;

   procedure Set_Exit_Status (Code : Exit_Status) is
   begin
      System.Standard_Library.gnat_exit_status := Integer (Code);
   end Set_Exit_Status;

end Ada.Command_Line;
