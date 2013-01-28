with Ada.Permissions.Inside;
package body Ada.Permissions is
   pragma Suppress (All_Checks);

   function User_Name return String is
   begin
      return Inside.User_Name (Inside.Current_User);
   end User_Name;

end Ada.Permissions;
