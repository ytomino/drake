with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.pwd;
with C.grp;
package body System.Native_Credentials is
   use type Storage_Elements.Storage_Offset;
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
            type gid_t_array is array (C.size_t) of aliased C.sys.types.gid_t
               with Convention => C;
            type gid_t_array_ptr is access all gid_t_array
               with Convention => C;
            package Conv is
               new Address_To_Named_Access_Conversions (
                  gid_t_array,
                  gid_t_array_ptr);
            procedure Finally (X : in out gid_t_array_ptr);
            procedure Finally (X : in out gid_t_array_ptr) is
            begin
               Standard_Allocators.Free (Conv.To_Address (X));
            end Finally;
            package Holder is
               new Ada.Exceptions.Finally.Scoped_Holder (
                  gid_t_array_ptr,
                  Finally);
            Capacity : C.signed_int := 16;
            Groups : aliased gid_t_array_ptr :=
               Conv.To_Pointer (
                  Standard_Allocators.Allocate (
                     Storage_Elements.Storage_Offset (Capacity)
                        * (C.sys.types.gid_t'Size / Standard'Storage_Unit)));
            Length : C.signed_int;
         begin
            Holder.Assign (Groups);
            loop
               Length := C.unistd.getgroups (Capacity, Groups (0)'Access);
               exit when Length >= 0;
               Capacity := Capacity * 2;
               Groups := Conv.To_Pointer (
                  System.Standard_Allocators.Reallocate (
                     Conv.To_Address (Groups),
                     System.Storage_Elements.Storage_Offset (Capacity)
                        * (C.sys.types.gid_t'Size / Standard'Storage_Unit)));
            end loop;
            for I in 0 .. Length - 1 loop
               if Id = Groups (C.size_t (I)) then
                  return True;
               end if;
            end loop;
            return False;
         end;
      end if;
   end Belongs_To_Current_Group;

end System.Native_Credentials;
