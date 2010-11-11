with System.To_String;
with C.pwd;
with C.grp;
package body Ada.Permissions.Inside is

   function User_Name (Id : C.sys.types.uid_t) return String is
      Info : C.pwd.struct_passwd_ptr;
   begin
      Info := C.pwd.getpwuid (Id);
      return System.To_String (Info.pw_name.all'Address);
   end User_Name;

   function Group_Name (Id : C.sys.types.gid_t) return String is
      Info : C.grp.struct_group_ptr;
   begin
      Info := C.grp.getgrgid (Id);
      return System.To_String (Info.gr_name.all'Address);
   end Group_Name;

end Ada.Permissions.Inside;
