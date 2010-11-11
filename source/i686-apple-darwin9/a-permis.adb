with C.unistd;
with Ada.Permissions.Inside;
package body Ada.Permissions is

   function User_Name return String is
   begin
      return Inside.User_Name (C.unistd.getuid);
   end User_Name;

end Ada.Permissions;
