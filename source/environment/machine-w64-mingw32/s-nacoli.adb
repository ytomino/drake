with System.Wide_Startup; -- force to be an unicode application
with System.Zero_Terminated_WStrings;
with C.winnt;
package body System.Native_Command_Line is

   function Argument (Number : Natural) return String is
      pragma Suppress (Alignment_Check);
      type wchar_t_ptr_Array is array (Natural) of C.winnt.LPCWSTR;
      wargv : wchar_t_ptr_Array
         with Import, Convention => C;
      for wargv'Address use Wide_Startup.wargv;
   begin
      return Zero_Terminated_WStrings.Value (wargv (Number));
   end Argument;

   function Argument_Count return Natural is
   begin
      return Wide_Startup.wargc - 1;
   end Argument_Count;

end System.Native_Command_Line;
