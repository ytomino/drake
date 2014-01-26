with System.Zero_Terminated_WStrings;
with C.winbase;
with C.windef;
with C.winnt;
package body Ada.Permissions.Inside is
   pragma Suppress (All_Checks);
   use type C.size_t;
   use type C.windef.WINBOOL;

   function User_Name return String is
      Result : aliased C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
      Length : aliased C.windef.DWORD := Result'Size;
   begin
      if C.winbase.GetUserName (
         Result (0)'Access,
         Length'Access) = 0
      then
         raise Constraint_Error; -- ???
      end if;
      return System.Zero_Terminated_WStrings.Value (
         Result (0)'Access,
         C.size_t (Length) - 1);
   end User_Name;

end Ada.Permissions.Inside;
