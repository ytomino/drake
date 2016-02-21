with System.Address_To_Constant_Access_Conversions;
with System.Wide_Startup; -- force to be an unicode application
with System.Zero_Terminated_WStrings;
with C.winnt;
package body System.Native_Command_Line is

   function Argument_Count return Natural is
   begin
      return Wide_Startup.wargc - 1;
   end Argument_Count;

   function Argument (Number : Natural) return String is
      type wchar_t_const_ptr_array is array (C.size_t) of C.winnt.LPCWSTR;
      type wchar_t_const_ptr_array_const_ptr is
         access constant wchar_t_const_ptr_array;
      package Conv is
         new System.Address_To_Constant_Access_Conversions (
            wchar_t_const_ptr_array,
            wchar_t_const_ptr_array_const_ptr);
   begin
      return Zero_Terminated_WStrings.Value (
         Conv.To_Pointer (Wide_Startup.wargv) (C.size_t (Number)));
   end Argument;

end System.Native_Command_Line;
