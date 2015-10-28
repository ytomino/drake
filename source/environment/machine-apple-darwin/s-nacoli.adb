with System.Startup;
with System.Zero_Terminated_Strings;
with C;
package body System.Native_Command_Line is

   function Argument_Count return Natural is
   begin
      return Startup.argc - 1;
   end Argument_Count;

   function Argument (Number : Natural) return String is
      pragma Suppress (Alignment_Check);
      argv : C.char_const_ptr_array (C.size_t)
         with Import, Convention => C;
      for argv'Address use Startup.argv;
   begin
      return Zero_Terminated_Strings.Value (argv (C.size_t (Number)));
   end Argument;

end System.Native_Command_Line;
