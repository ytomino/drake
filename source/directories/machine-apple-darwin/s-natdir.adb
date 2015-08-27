with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with Ada.Hierarchical_File_Names;
with System.Address_To_Named_Access_Conversions;
with System.Native_Directories.Copying;
with System.Zero_Terminated_Strings;
with C.errno;
with C.stdio; -- rename(2)
with C.stdlib;
with C.sys.time;
with C.time;
with C.unistd;
package body System.Native_Directories is
   use Ada.Exception_Identification.From_Here;
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
         Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      Error := C.sys.stat.lstat (C_Name (0)'Access, Information'Access) < 0;
   end Get_Information;

   --  implementation

   function Current_Directory return String is
      procedure Finally (X : not null access C.char_ptr);
      procedure Finally (X : not null access C.char_ptr) is
         package Conv is
            new Address_To_Named_Access_Conversions (C.char, C.char_ptr);
      begin
         C.stdlib.free (C.void_ptr (Conv.To_Address (X.all)));
      end Finally;
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (C.char_ptr, Finally);
      Path : aliased C.char_ptr := C.unistd.getcwd (null, 0);
   begin
      Holder.Assign (Path'Access);
      return Zero_Terminated_Strings.Value (Path);
   end Current_Directory;

   procedure Set_Directory (Directory : String) is
      C_Directory : C.char_array (
         0 ..
         Directory'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Directory, C_Directory (0)'Access);
      if C.unistd.chdir (C_Directory (0)'Access) /= 0 then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Set_Directory;

   procedure Create_Directory (New_Directory : String) is
      C_New_Directory : C.char_array (
         0 ..
         New_Directory'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (New_Directory, C_New_Directory (0)'Access);
      if C.sys.stat.mkdir (C_New_Directory (0)'Access, 8#755#) /= 0 then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Create_Directory;

   procedure Delete_Directory (Directory : String) is
      C_Directory : C.char_array (
         0 ..
         Directory'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Directory, C_Directory (0)'Access);
      if C.unistd.rmdir (C_Directory (0)'Access) < 0 then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Delete_Directory;

   procedure Delete_File (Name : String) is
      C_Name : C.char_array (
         0 ..
         Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      if C.unistd.unlink (C_Name (0)'Access) < 0 then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Delete_File;

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean)
   is
      C_Old_Name : C.char_array (
         0 ..
         Old_Name'Length * Zero_Terminated_Strings.Expanding);
      C_New_Name : C.char_array (
         0 ..
         New_Name'Length * Zero_Terminated_Strings.Expanding);
      Error : Boolean;
   begin
      Zero_Terminated_Strings.To_C (Old_Name, C_Old_Name (0)'Access);
      Zero_Terminated_Strings.To_C (New_Name, C_New_Name (0)'Access);
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
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Rename;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean)
      renames Copying.Copy_File;

   procedure Replace_File (
      Source_Name : String;
      Target_Name : String)
      renames Copying.Replace_File;

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean)
   is
      C_Source_Name : C.char_array (
         0 ..
         Source_Name'Length * Zero_Terminated_Strings.Expanding);
      C_Target_Name : C.char_array (
         0 ..
         Target_Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Source_Name, C_Source_Name (0)'Access);
      Zero_Terminated_Strings.To_C (Target_Name, C_Target_Name (0)'Access);
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
            Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
         end if;
      end;
   end Symbolic_Link;

   function Full_Name (Name : String) return String is
   begin
      if Ada.Hierarchical_File_Names.Is_Relative_Name (Name) then
         return Ada.Hierarchical_File_Names.Unfolded_Compose (
            Current_Directory,
            Name);
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
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Get_Information;

   function Kind (mode : C.sys.types.mode_t) return File_Kind is
      Masked_Type : constant C.sys.types.mode_t := mode and C.sys.stat.S_IFMT;
   begin
      if Masked_Type = C.sys.stat.S_IFDIR then
         return Directory;
      elsif Masked_Type = C.sys.stat.S_IFREG then
         return Ordinary_File;
      else
         return Special_File;
      end if;
   end Kind;

   function Kind (Information : Directory_Entry_Information_Type)
      return File_Kind is
   begin
      return File_Kind'Enum_Val (
         File_Kind'Enum_Rep (Kind (Information.st_mode)));
   end Kind;

   function Size (Information : Directory_Entry_Information_Type)
      return Ada.Streams.Stream_Element_Count is
   begin
      return Ada.Streams.Stream_Element_Count (Information.st_size);
   end Size;

   function Modification_Time (Information : Directory_Entry_Information_Type)
      return Native_Calendar.Native_Time is
   begin
      return Information.st_mtim;
   end Modification_Time;

   procedure Set_Modification_Time (
      Name : String;
      Time : Native_Calendar.Native_Time)
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
         Name'Length * Zero_Terminated_Strings.Expanding);
      Attributes : aliased C.sys.stat.struct_stat;
      Times : aliased array (0 .. 1) of aliased C.sys.time.struct_timeval;
      Error : Boolean;
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      Error := C.sys.stat.lstat (C_Name (0)'Access, Attributes'Access) < 0;
      if not Error then
         Times (0) := To_timeval (Attributes.st_atim);
         Times (1) := To_timeval (Time);
         Error := C.sys.time.lutimes (C_Name (0)'Access, Times (0)'Access) < 0;
      end if;
      if Error then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Set_Modification_Time;

   function IO_Exception_Id (errno : C.signed_int)
      return Ada.Exception_Identification.Exception_Id is
   begin
      case errno is
         when C.errno.EIO =>
            return Device_Error'Identity;
         when others =>
            return Use_Error'Identity;
      end case;
   end IO_Exception_Id;

   function Named_IO_Exception_Id (errno : C.signed_int)
      return Ada.Exception_Identification.Exception_Id is
   begin
      case errno is
         when C.errno.ENOENT
            | C.errno.ENOTDIR
            | C.errno.EISDIR
            | C.errno.ENAMETOOLONG =>
            return Name_Error'Identity;
         when others =>
            return IO_Exception_Id (errno);
      end case;
   end Named_IO_Exception_Id;

end System.Native_Directories;
