with System.Native_Credentials;
package body Ada.Credentials is
   pragma Suppress (All_Checks);

   function User_Name return String is
   begin
      --  User_Name has a default parameter in Darwin, but not in Windows
      return System.Native_Credentials.User_Name;
   end User_Name;

end Ada.Credentials;
