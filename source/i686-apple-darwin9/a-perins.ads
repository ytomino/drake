pragma License (Unrestricted);
--  implementation unit
with C.sys.types;
with C.unistd;
package Ada.Permissions.Inside is
   pragma Preelaborate;

   subtype User_Id is C.sys.types.uid_t;
   subtype Group_Id is C.sys.types.gid_t;

   function User_Name (Id : User_Id) return String;
   function Group_Name (Id : Group_Id) return String;

   function Current_User return User_Id
      renames C.unistd.getuid;

end Ada.Permissions.Inside;
