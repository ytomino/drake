with System.Zero_Terminated_Strings;
with C.pwd;
with C.grp;
package body System.Native_Credentials is
   use type C.pwd.struct_passwd_ptr;
   use type C.grp.struct_group_ptr;

   function User_Name (Id : User_Id := Current_User) return String is
      Info : C.pwd.struct_passwd_ptr;
   begin
      Info := C.pwd.getpwuid (Id);
      if Info = null then
         raise Constraint_Error;
      else
         return Zero_Terminated_Strings.Value (Info.pw_name);
      end if;
   end User_Name;

   function Group_Name (Id : Group_Id := Current_Group) return String is
      Info : C.grp.struct_group_ptr;
   begin
      Info := C.grp.getgrgid (Id);
      if Info = null then
         raise Constraint_Error;
      else
         return Zero_Terminated_Strings.Value (Info.gr_name);
      end if;
   end Group_Name;

end System.Native_Credentials;
