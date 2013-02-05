with Ada.Directories.Inside.Do_Copy_File;
with Ada.Exceptions;
with C.errno;
with C.stdlib;
with C.string;
with C.sys.time;
with C.sys.types;
with C.time;
with C.unistd;
package body Ada.Directories.Inside is
   use type C.signed_int;
   use type C.signed_long;
   use type C.sys.types.mode_t;

   --  reference:
   --  http://www.opensource.apple.com/source/gcc/gcc-5664/libiberty/rename.c
   function C_rename (
      zfrom : access constant C.char;
      zto : access constant C.char;
      Overwrite : Boolean)
      return C.signed_int;
   pragma Convention (C, C_rename);
   function C_rename (
      zfrom : access constant C.char;
      zto : access constant C.char;
      Overwrite : Boolean)
      return C.signed_int is
   begin
      if C.unistd.link (zfrom, zto) < 0 then
         if C.errno.errno /= C.errno.EEXIST or else not Overwrite then
            return -1;
         end if;
         --  try to overwrite
         if C.unistd.unlink (zto) < 0
            or else C.unistd.link (zfrom, zto) < 0
         then
            return -1;
         end if;
      end if;
      return C.unistd.unlink (zfrom);
   end C_rename;

   procedure Get_Information (
      Name : String;
      Information : not null access Directory_Entry_Information_Type;
      Error : out Boolean);
   procedure Get_Information (
      Name : String;
      Information : not null access Directory_Entry_Information_Type;
      Error : out Boolean)
   is
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      Error := C.sys.stat.lstat (C_Name (0)'Access, Information) < 0;
   end Get_Information;

   --  implementation

   function Current_Directory return String is
      Path : constant C.char_ptr := C.unistd.getcwd (null, 0);
      Path_Length : constant C.size_t := C.string.strlen (Path);
      Path_String : String (1 .. Natural (Path_Length));
      for Path_String'Address use Path.all'Address;
   begin
      return Result : String := Path_String do -- copy
         pragma Unmodified (Result);
         pragma Unreferenced (Result);
         C.stdlib.free (C.void_ptr (Path.all'Address));
      end return;
   end Current_Directory;

   procedure Set_Directory (Directory : String) is
      Z_Directory : constant String := Directory & Character'Val (0);
      C_Directory : C.char_array (C.size_t);
      for C_Directory'Address use Z_Directory'Address;
   begin
      if C.unistd.chdir (C_Directory (0)'Access) /= 0 then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
   end Set_Directory;

   procedure Create_Directory (
      New_Directory : String;
      Form : String)
   is
      pragma Unreferenced (Form);
      Z_New_Directory : constant String := New_Directory & Character'Val (0);
      C_New_Directory : C.char_array (C.size_t);
      for C_New_Directory'Address use Z_New_Directory'Address;
   begin
      if C.sys.stat.mkdir (C_New_Directory (0)'Access, 8#755#) /= 0 then
         case C.errno.errno is
            when C.errno.ENOENT
               | C.errno.ENOTDIR
               | C.errno.ENAMETOOLONG =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
   end Create_Directory;

   procedure Delete_Directory (Directory : String) is
      Z_Directory : constant String := Directory & Character'Val (0);
      C_Directory : C.char_array (C.size_t);
      for C_Directory'Address use Z_Directory'Address;
   begin
      if C.unistd.rmdir (C_Directory (0)'Access) < 0 then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
   end Delete_Directory;

   procedure Delete_File (Name : String) is
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      if C.unistd.unlink (C_Name (0)'Access) < 0 then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
   end Delete_File;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Form : String;
      Overwrite : Boolean)
      renames Inside.Do_Copy_File;

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean)
   is
      Z_Old : constant String := Old_Name & Character'Val (0);
      C_Old : C.char_array (C.size_t);
      for C_Old'Address use Z_Old'Address;
      Z_New : constant String := New_Name & Character'Val (0);
      C_New : C.char_array (C.size_t);
      for C_New'Address use Z_New'Address;
   begin
      if C_rename (C_Old (0)'Access, C_New (0)'Access, Overwrite) < 0 then
         case C.errno.errno is
            when C.errno.ENOENT
               | C.errno.ENOTDIR
               | C.errno.EISDIR
               | C.errno.ENAMETOOLONG =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
   end Rename;

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean)
   is
      Z_Source : constant String := Source_Name & Character'Val (0);
      C_Source : C.char_array (C.size_t);
      for C_Source'Address use Z_Source'Address;
      Z_Target : constant String := Target_Name & Character'Val (0);
      C_Target : C.char_array (C.size_t);
      for C_Target'Address use Z_Target'Address;
      path1 : constant not null access constant C.char := C_Source (0)'Access;
      path2 : constant not null access constant C.char := C_Target (0)'Access;
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
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
   end Symbolic_Link;

   function Exists (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
      Error : Boolean;
   begin
      Get_Information (Name, Information'Access, Error);
      return not Error;
   end Exists;

   procedure Get_Information (
      Name : String;
      Information : not null access Directory_Entry_Information_Type)
   is
      Error : Boolean;
   begin
      Get_Information (Name, Information, Error);
      if Error then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
   end Get_Information;

   function Kind (Information : Directory_Entry_Information_Type)
      return File_Kind
   is
      Kind_Attr : constant C.sys.types.mode_t :=
         Information.st_mode and C.sys.stat.S_IFMT;
   begin
      if Kind_Attr = C.sys.stat.S_IFDIR then
         return Directory;
      elsif Kind_Attr = C.sys.stat.S_IFREG then
         return Ordinary_File;
      else
         return Special_File;
      end if;
   end Kind;

   function Size (Information : Directory_Entry_Information_Type)
      return File_Size is
   begin
      return File_Size (Information.st_size);
   end Size;

   function Modification_Time (Information : Directory_Entry_Information_Type)
      return System.Native_Time.Native_Time is
   begin
      return Information.st_mtimespec;
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
      Z_Name : constant String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
      Attributes : aliased C.sys.stat.struct_stat;
      Times : aliased array (0 .. 1) of aliased C.sys.time.struct_timeval;
   begin
      if C.sys.stat.lstat (C_Name (0)'Access, Attributes'Access) < 0 then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
      Times (0) := To_timeval (Attributes.st_atimespec);
      Times (1) := To_timeval (Time);
      if C.sys.time.lutimes (C_Name (0)'Access, Times (0)'Access) < 0 then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
   end Set_Modification_Time;

end Ada.Directories.Inside;
