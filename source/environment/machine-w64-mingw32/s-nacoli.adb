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
      type Fixed_LPCWSTR_array is array (C.size_t) of C.winnt.LPCWSTR
         with Convention => C;
      type LPCWSTR_array_const_ptr is access constant Fixed_LPCWSTR_array
         with Convention => C;
      package Conv is
         new Address_To_Constant_Access_Conversions (
            Fixed_LPCWSTR_array,
            LPCWSTR_array_const_ptr);
   begin
      return Zero_Terminated_WStrings.Value (
         Conv.To_Pointer (Wide_Startup.wargv) (C.size_t (Number)));
   end Argument;

end System.Native_Command_Line;
