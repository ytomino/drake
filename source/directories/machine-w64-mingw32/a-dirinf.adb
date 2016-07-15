with Ada.Exception_Identification.From_Here;
with Ada.Unchecked_Conversion;
with System.Native_Calendar;
with System.Zero_Terminated_WStrings;
with C.winbase;
with C.winnt;
package body Ada.Directories.Information is
   use Exception_Identification.From_Here;
   use type Exception_Identification.Exception_Id;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.winnt.HANDLE; -- C.void_ptr

   subtype Directory_Entry_Information_Type is
      System.Native_Directories.Directory_Entry_Information_Type;

   function IO_Exception_Id (errno : C.windef.DWORD)
      return Exception_Identification.Exception_Id
      renames System.Native_Directories.IO_Exception_Id;

   function Named_IO_Exception_Id (errno : C.windef.DWORD)
      return Exception_Identification.Exception_Id
      renames System.Native_Directories.Named_IO_Exception_Id;

   function Cast is new Unchecked_Conversion (Duration, Calendar.Time);

   function Is_Executable (Name : String) return Boolean;
   function Is_Executable (Name : String) return Boolean is
      --  Currently, check only that an extension is ".COM" or ".EXE".
      --  It should check PATHEXT, or look PE header?
      Ext_First : Positive;
      Ext_Last : Natural;
      Result : Boolean;
   begin
      Hierarchical_File_Names.Extension (Name,
         First => Ext_First, Last => Ext_Last);
      if Ext_Last - Ext_First + 1 = 3 then
         declare
            Ext : String (1 .. 3) := Name (Ext_First .. Ext_Last);
         begin
            for I in Ext'Range loop
               if Ext (I) in 'a' .. 'z' then
                  Ext (I) := Character'Val (Character'Pos (Ext (I)) - 16#20#);
               end if;
            end loop;
            Result := Ext = "COM" or else Ext = "EXE";
         end;
      else
         Result := False;
      end if;
      return Result;
   end Is_Executable;

   --  implementation

   function Creation_Time (Name : String) return Calendar.Time is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return Cast (
         System.Native_Calendar.To_Time (Information.ftCreationTime));
   end Creation_Time;

   function Last_Access_Time (Name : String) return Calendar.Time is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return Cast (
         System.Native_Calendar.To_Time (Information.ftLastAccessTime));
   end Last_Access_Time;

   function Is_Read_Only (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SPARSE_FILE) /= 0;
   end Is_Read_Only;

   function Needs_Archiving (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_ARCHIVE) /= 0;
   end Needs_Archiving;

   function Is_Compressed (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_COMPRESSED) /= 0;
   end Is_Compressed;

   function Is_Encrypted (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_ENCRYPTED) /= 0;
   end Is_Encrypted;

   function Is_Hidden (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_HIDDEN) /= 0;
   end Is_Hidden;

   function Is_System (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SYSTEM) /= 0;
   end Is_System;

   function Is_Offline (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_OFFLINE) /= 0;
   end Is_Offline;

   function Is_Temporary (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_TEMPORARY) /= 0;
   end Is_Temporary;

   function Is_Sparse (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SPARSE_FILE) /= 0;
   end Is_Sparse;

   function Is_Not_Indexed (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_NOT_CONTENT_INDEXED) /= 0;
   end Is_Not_Indexed;

   function Creation_Time (
      Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return Cast (
         System.Native_Calendar.To_Time (
            NC_Directory_Entry.Directory_Entry.ftLastWriteTime));
   end Creation_Time;

   function Last_Access_Time (
      Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return Cast (
         System.Native_Calendar.To_Time (
            NC_Directory_Entry.Directory_Entry.ftLastAccessTime));
   end Last_Access_Time;

   function Is_Read_Only (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_READONLY) /= 0;
   end Is_Read_Only;

   function Needs_Archiving (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_ARCHIVE) /= 0;
   end Needs_Archiving;

   function Is_Compressed (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_COMPRESSED) /= 0;
   end Is_Compressed;

   function Is_Encrypted (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_ENCRYPTED) /= 0;
   end Is_Encrypted;

   function Is_Hidden (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_HIDDEN) /= 0;
   end Is_Hidden;

   function Is_System (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SYSTEM) /= 0;
   end Is_System;

   function Is_Offline (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_OFFLINE) /= 0;
   end Is_Offline;

   function Is_Temporary (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_TEMPORARY) /= 0;
   end Is_Temporary;

   function Is_Sparse (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_SPARSE_FILE) /= 0;
   end Is_Sparse;

   function Is_Not_Indexed (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_NOT_CONTENT_INDEXED) /= 0;
   end Is_Not_Indexed;

   function Is_Symbolic_Link (Name : String) return Boolean is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return (Information.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_REPARSE_POINT) /= 0;
   end Is_Symbolic_Link;

   function Is_Symbolic_Link (
      Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled.Reference (Directory_Entry).all;
   begin
      return (NC_Directory_Entry.Directory_Entry.dwFileAttributes
         and C.winnt.FILE_ATTRIBUTE_REPARSE_POINT) /= 0;
   end Is_Symbolic_Link;

   function User_Permission_Set (Name : String)
      return User_Permission_Set_Type
   is
      Executable : constant Boolean := Is_Executable (Name);
      Writable : constant Boolean := not Is_Read_Only (Name);
      Readable : constant Boolean := True;
   begin
      return (
         User_Execute => Executable,
         User_Write => Writable,
         User_Read => Readable);
   end User_Permission_Set;

   function User_Permission_Set (
      Directory_Entry : Directory_Entry_Type) -- Assigned_Directory_Entry_Type
      return User_Permission_Set_Type
   is
      Executable : constant Boolean :=
         Is_Executable (
            Simple_Name (Directory_Entry)); -- checking the predicate
      Writable : constant Boolean := not Is_Read_Only (Directory_Entry);
      Readable : constant Boolean := True;
   begin
      return (
         User_Execute => Executable,
         User_Write => Writable,
         User_Read => Readable);
   end User_Permission_Set;

   function Identity (Name : String) return File_Id is
      Exception_Id : Exception_Identification.Exception_Id :=
         Exception_Identification.Null_Id;
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * System.Zero_Terminated_WStrings.Expanding);
      Handle : C.winnt.HANDLE;
      Info : aliased C.winbase.BY_HANDLE_FILE_INFORMATION;
   begin
      System.Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      Handle := C.winbase.CreateFile (
         W_Name (0)'Access,
         dwDesiredAccess => 0,
         dwShareMode =>
            C.winnt.FILE_SHARE_READ or C.winnt.FILE_SHARE_WRITE
            or C.winnt.FILE_SHARE_DELETE, -- only for query
         lpSecurityAttributes => null,
         dwCreationDisposition => C.winbase.OPEN_EXISTING,
         dwFlagsAndAttributes =>
            C.winbase.FILE_FLAG_BACKUP_SEMANTICS
            or C.winbase.FILE_FLAG_OPEN_REPARSE_POINT,
         hTemplateFile => C.windef.LPVOID (System.Null_Address));
      if Handle = C.winbase.INVALID_HANDLE_VALUE then
         Exception_Id := Named_IO_Exception_Id (C.winbase.GetLastError);
      else
         if C.winbase.GetFileInformationByHandle (Handle, Info'Access) =
            C.windef.FALSE
         then
            Exception_Id := IO_Exception_Id (C.winbase.GetLastError);
         end if;
         if C.winbase.CloseHandle (Handle) = C.windef.FALSE then
            if Exception_Id = Exception_Identification.Null_Id then
               Exception_Id := IO_Exception_Id (C.winbase.GetLastError);
            end if;
         end if;
      end if;
      if Exception_Id /= Exception_Identification.Null_Id then
         Raise_Exception (Exception_Id);
      end if;
      return (
         FileIndexLow => Info.nFileIndexLow,
         FileIndexHigh => Info.nFileIndexHigh,
         VolumeSerialNumber => Info.dwVolumeSerialNumber);
   end Identity;

   function Identity (
      Directory_Entry : Directory_Entry_Type)
      return File_Id is
   begin
      --  WIN32_FILE_ATTRIBUTE_DATA does not contain the file index
      return Identity (Full_Name (Directory_Entry)); -- checking the predicate
   end Identity;

end Ada.Directories.Information;
