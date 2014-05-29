with System.Wide_Startup; -- force to be an unicode application
with System.Zero_Terminated_WStrings;
with C.winnt;
package body Ada.Command_Line.Inside is
   pragma Suppress (All_Checks);

   function Argument (Number : Natural) return String is
      type wchar_t_ptr_Array is array (Natural) of C.winnt.LPCWSTR;
      wargv : wchar_t_ptr_Array;
      pragma Import (C, wargv);
      for wargv'Address use System.Wide_Startup.wargv;
   begin
      return System.Zero_Terminated_WStrings.Value (wargv (Number));
   end Argument;

   function Argument_Count return Natural is
   begin
      return System.Wide_Startup.wargc - 1;
   end Argument_Count;

end Ada.Command_Line.Inside;
