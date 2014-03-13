pragma License (Unrestricted);
--  implementation unit
with C.sys.types;
with C.unistd;
package System.Native_Credentials is
   pragma Preelaborate;

   subtype User_Id is C.sys.types.uid_t;
   subtype Group_Id is C.sys.types.gid_t;

   function Current_User return User_Id
      renames C.unistd.getuid;

   function User_Name (Id : User_Id := Current_User) return String;
   function Group_Name (Id : Group_Id := Current_User) return String;

end System.Native_Credentials;
