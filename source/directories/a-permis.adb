with Ada.Permissions.Inside;
package body Ada.Permissions is
   pragma Suppress (All_Checks);

   function User_Name return String is
   begin
      --  User_Name has a default parameter in Darwin, but not in Windows
      return Inside.User_Name;
   end User_Name;

end Ada.Permissions;
