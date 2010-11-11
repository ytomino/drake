pragma License (Unrestricted);
--  implementation package
with C.sys.types;
package Ada.Permissions.Inside is
   pragma Preelaborate;

   function User_Name (Id : C.sys.types.uid_t) return String;
   function Group_Name (Id : C.sys.types.gid_t) return String;

end Ada.Permissions.Inside;
