with Ada.Directories.Copying;
with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Zero_Terminated_Strings;
with C.errno;
with C.stdio; -- rename(2)
with C.stdlib;
with C.sys.time;
with C.sys.types;
with C.time;
with C.unistd;
package body Ada.Directories.Inside is
   use Exception_Identification.From_Here;
   use type C.signed_int;
   use type C.signed_long;
   use type C.size_t;
   use type C.sys.types.mode_t;

   procedure Get_Information (
      Name : String;
      Information : aliased out Directory_Entry_Information_Type;
      Error : out Boolean);
   procedure Get_Information (
      Name : String;
      Information : aliased out Directory_Entry_Information_Type;
      Error : out Boolean)
   is
      C_Name : C.char_array (
         0 ..
         Name'Length * System.Zero_Terminated_Strings.Expanding);
   begin
      System.Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      Error := C.sys.stat.lstat (C_Name (0)'Access, Information'Access) < 0;
   end Get_Information;

   --  implementation

   function Current_Directory return String is
      procedure Finally (X : not null access C.char_ptr);
      procedure Finally (X : not null access C.char_ptr) is
         package Conv is
            new System.Address_To_Named_Access_Conversions (
               C.char,
               C.char_ptr);
      begin
         C.stdlib.free (C.void_ptr (Conv.To_Address (X.all)));
      end Finally;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (C.char_ptr, Finally);
      Path : aliased C.char_ptr := C.unistd.getcwd (null, 0);
   begin
      Holder.Assign (Path'Access);
      return System.Zero_Terminated_Strings.Value (Path);
   end Current_Directory;

   procedure Set_Directory (Directory : String) is
      C_Directory : C.char_array (
         0 ..
         Directory'Length * System.Zero_Terminated_Strings.Expanding);
   begin
      System.Zero_Terminated_Strings.To_C (Directory, C_Directory (0)'Access);
      if C.unistd.chdir (C_Directory (0)'Access) /= 0 then
         Raise_Exception (Name_Error'Identity);
      end if;
   end Set_Directory;

   procedure Create_Directory (
      New_Directory : String;
      Form : String)
   is
      pragma Unreferenced (Form);
      C_New_Directory : C.char_array (
         0 ..
         New_Directory'Length * System.Zero_Terminated_Strings.Expanding);
   begin
      System.Zero_Terminated_Strings.To_C (
         New_Directory,
         C_New_Directory (0)'Access);
      if C.sys.stat.mkdir (C_New_Directory (0)'Access, 8#755#) /= 0 then
         case C.errno.errno is
            when C.errno.ENOENT
               | C.errno.ENOTDIR
               | C.errno.ENAMETOOLONG =>
               Raise_Exception (Name_Error'Identity);
            when others =>
               Raise_Exception (Use_Error'Identity);
         end case;
      end if;
   end Create_Directory;

   procedure Delete_Directory (Directory : String) is
      C_Directory : C.char_array (
         0 ..
         Directory'Length * System.Zero_Terminated_Strings.Expanding);
   begin
      System.Zero_Terminated_Strings.To_C (Directory, C_Directory (0)'Access);
      if C.unistd.rmdir (C_Directory (0)'Access) < 0 then
         Raise_Exception (Name_Error'Identity);
      end if;
   end Delete_Directory;

   procedure Delete_File (Name : String) is
      C_Name : C.char_array (
         0 ..
         Name'Length * System.Zero_Terminated_Strings.Expanding);
   begin
      System.Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      if C.unistd.unlink (C_Name (0)'Access) < 0 then
         Raise_Exception (Name_Error'Identity);
      end if;
   end Delete_File;

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean)
   is
      C_Old_Name : C.char_array (
         0 ..
         Old_Name'Length * System.Zero_Terminated_Strings.Expanding);
      C_New_Name : C.char_array (
         0 ..
         New_Name'Length * System.Zero_Terminated_Strings.Expanding);
      Error : Boolean;
   begin
      System.Zero_Terminated_Strings.To_C (Old_Name, C_Old_Name (0)'Access);
      System.Zero_Terminated_Strings.To_C (New_Name, C_New_Name (0)'Access);
      if Overwrite then
         Error := C.stdio.rename (
            C_Old_Name (0)'Access,
            C_New_Name (0)'Access) < 0;
      else
         Error := C.unistd.link (
            C_Old_Name (0)'Access,
            C_New_Name (0)'Access) < 0;
         if not Error then
            Error := C.unistd.unlink (C_Old_Name (0)'Access) < 0;
         end if;
      end if;
      if Error then
         case C.errno.errno is
            when C.errno.ENOENT
               | C.errno.ENOTDIR
               | C.errno.EISDIR
               | C.errno.ENAMETOOLONG =>
               Raise_Exception (Name_Error'Identity);
            when others =>
               Raise_Exception (Use_Error'Identity);
         end case;
      end if;
   end Rename;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean)
      renames Copying.Copy_File;

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean)
   is
      C_Source_Name : C.char_array (
         0 ..
         Source_Name'Length * System.Zero_Terminated_Strings.Expanding);
      C_Target_Name : C.char_array (
         0 ..
         Target_Name'Length * System.Zero_Terminated_Strings.Expanding);
   begin
      System.Zero_Terminated_Strings.To_C (
         Source_Name,
         C_Source_Name (0)'Access);
      System.Zero_Terminated_Strings.To_C (
         Target_Name,
         C_Target_Name (0)'Access);
      declare
         path1 : constant not null access constant C.char :=
            C_Source_Name (0)'Access;
         path2 : constant not null access constant C.char :=
            C_Target_Name (0)'Access;
      begin
         if C.unistd.symlink (path1, path2) < 0 then
            if C.errno.errno = C.errno.EEXIST and then Overwrite then
               --  try to overwrite
               if C.unistd.unlink (path2) = 0
                  and then C.unistd.symlink (path1, path2) = 0
               then
                  return; -- success
               end if;
            end if;
            case C.errno.errno is
               when C.errno.ENOENT
                  | C.errno.ENOTDIR
                  | C.errno.ENAMETOOLONG =>
                  Raise_Exception (Name_Error'Identity);
               when others =>
                  Raise_Exception (Use_Error'Identity);
            end case;
         end if;
      end;
   end Symbolic_Link;

   function Full_Name (Name : String) return String is
   begin
      if Hierarchical_File_Names.Is_Relative_Name (Name) then
         return Compose (Current_Directory, Name);
      else
         return Name;
      end if;
   end Full_Name;

   function Exists (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
      Error : Boolean;
   begin
      Get_Information (Name, Information, Error);
      return not Error;
   end Exists;

   procedure Get_Information (
      Name : String;
      Information : aliased out Directory_Entry_Information_Type)
   is
      Error : Boolean;
   begin
      Get_Information (Name, Information, Error);
      if Error then
         Raise_Exception (Name_Error'Identity);
      end if;
   end Get_Information;

   function Kind (Information : Directory_Entry_Information_Type)
      return File_Kind is
   begin
      return File_Kind'Enum_Val (Directory_Searching.File_Kind'Enum_Rep (
         Directory_Searching.To_File_Kind (Information.st_mode)));
   end Kind;

   function Size (Information : Directory_Entry_Information_Type)
      return File_Size is
   begin
      return File_Size (Information.st_size);
   end Size;

   function Modification_Time (Information : Directory_Entry_Information_Type)
      return System.Native_Time.Native_Time is
   begin
      return Information.st_mtim;
   end Modification_Time;

   procedure Set_Modification_Time (
      Name : String;
      Time : System.Native_Time.Native_Time)
   is
      function To_timeval (X : C.time.struct_timespec)
         return C.sys.time.struct_timeval;
      function To_timeval (X : C.time.struct_timespec)
         return C.sys.time.struct_timeval is
      begin
         return (
            tv_sec => X.tv_sec,
            tv_usec => C.sys.types.suseconds_t (X.tv_nsec / 1000));
      end To_timeval;
      C_Name : C.char_array (
         0 ..
         Name'Length * System.Zero_Terminated_Strings.Expanding);
      Attributes : aliased C.sys.stat.struct_stat;
      Times : aliased array (0 .. 1) of aliased C.sys.time.struct_timeval;
   begin
      System.Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      if C.sys.stat.lstat (C_Name (0)'Access, Attributes'Access) < 0 then
         Raise_Exception (Name_Error'Identity);
      end if;
      Times (0) := To_timeval (Attributes.st_atim);
      Times (1) := To_timeval (Time);
      if C.sys.time.lutimes (C_Name (0)'Access, Times (0)'Access) < 0 then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Set_Modification_Time;

end Ada.Directories.Inside;
