with System.Address_To_Constant_Access_Conversions;
with System.Startup;
with System.Zero_Terminated_Strings;
with C;
package body System.Native_Command_Line is

   function Argument_Count return Natural is
   begin
      return Startup.argc - 1;
   end Argument_Count;

   function Argument (Number : Natural) return String is
      subtype Fixed_char_const_ptr_array is C.char_const_ptr_array (C.size_t);
      type char_const_ptr_array_const_ptr is
         access constant Fixed_char_const_ptr_array;
      package Conv is
         new Address_To_Constant_Access_Conversions (
            Fixed_char_const_ptr_array,
            char_const_ptr_array_const_ptr);
   begin
      return Zero_Terminated_Strings.Value (
         Conv.To_Pointer (Startup.argv) (C.size_t (Number)));
   end Argument;

end System.Native_Command_Line;
