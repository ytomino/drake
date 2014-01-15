with System.Standard_Library;
with System.Zero_Terminated_Strings;
with C;
package body Ada.Command_Line.Inside is
   pragma Suppress (All_Checks);

   function Argument (Number : Natural) return String is
      argv : C.char_const_ptr_array (C.size_t);
      pragma Import (C, argv);
      for argv'Address use System.Standard_Library.gnat_argv;
   begin
      return System.Zero_Terminated_Strings.Value (argv (C.size_t (Number)));
   end Argument;

   function Argument_Count return Natural is
   begin
      return System.Standard_Library.gnat_argc - 1;
   end Argument_Count;

end Ada.Command_Line.Inside;
