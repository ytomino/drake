with System.Standard_Library;
with System.Zero_Terminated_Strings;
package body Ada.Command_Line.Inside is
   pragma Suppress (All_Checks);

   function Argument (Number : Natural) return String is
      type String_Array is array (Natural) of System.Address;
      argv : String_Array;
      for argv'Address use System.Standard_Library.gnat_argv;
   begin
      return System.Zero_Terminated_Strings.Value (argv (Number));
   end Argument;

   function Argument_Count return Natural is
   begin
      return System.Standard_Library.gnat_argc - 1;
   end Argument_Count;

end Ada.Command_Line.Inside;
