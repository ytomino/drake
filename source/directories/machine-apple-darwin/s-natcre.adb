with System.Address_To_Named_Access_Conversions;
with System.Growth;
with System.Zero_Terminated_Strings;
with C.grp;
with C.pwd;
package body System.Native_Credentials is
   use type C.pwd.struct_passwd_ptr;
   use type C.grp.struct_group_ptr;
   use type C.signed_int;
--  use type C.sys.types.gid_t;
--  use type C.sys.types.uid_t;

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

   function Belongs_To_Current_User (Id : User_Id) return Boolean is
      use type C.sys.types.uid_t;
   begin
      return Id = C.unistd.getuid;
   end Belongs_To_Current_User;

   function Belongs_To_Current_Group (Id : Group_Id) return Boolean is
      use type C.sys.types.gid_t;
   begin
      if Id = C.unistd.getgid then
         return True;
      else
         declare
            type gid_t_array is
               array (C.size_t range <>) of aliased C.sys.types.gid_t
               with Convention => C;
            type gid_t_ptr is access all C.sys.types.gid_t
               with Convention => C;
            package gid_t_ptr_Conv is
               new Address_To_Named_Access_Conversions (
                  C.sys.types.gid_t,
                  gid_t_ptr);
            package Holder is
               new Growth.Scoped_Holder (
                  C.signed_int,
                  Component_Size => gid_t_array'Component_Size);
            Length : C.signed_int;
         begin
            Holder.Reserve_Capacity (16);
            loop
               Length := C.unistd.getgroups (
                  Holder.Capacity,
                  gid_t_ptr_Conv.To_Pointer (Holder.Storage_Address));
               exit when Length >= 0;
               --  growth
               declare
                  function Grow is new Growth.Fast_Grow (C.signed_int);
               begin
                  Holder.Reserve_Capacity (Grow (Holder.Capacity));
               end;
            end loop;
            declare
               Groups : gid_t_array (
                  0 .. C.size_t (C.signed_int'Max (0, Length - 1)));
               for Groups'Address use Holder.Storage_Address;
            begin
               for I in 0 .. Length - 1 loop
                  if Id = Groups (C.size_t (I)) then
                     return True;
                  end if;
               end loop;
            end;
            return False;
         end;
      end if;
   end Belongs_To_Current_Group;

end System.Native_Credentials;
