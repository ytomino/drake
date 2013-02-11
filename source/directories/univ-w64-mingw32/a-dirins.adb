with Ada.Exceptions;
with System.Zero_Terminated_WStrings;
with C.windef;
with C.winerror;
with C.winnt;
package body Ada.Directories.Inside is
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.winnt.HANDLE;

   function Current_Directory return String is
      Buffer : C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
      Length : C.windef.DWORD;
   begin
      Length := C.winbase.GetCurrentDirectory (
         C.windef.MAX_PATH,
         Buffer (0)'Access);
      if Length = 0 then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      else
         return System.Zero_Terminated_WStrings.Value (
            Buffer (0)'Access,
            C.signed_int (Length));
      end if;
   end Current_Directory;

   procedure Set_Directory (Directory : String) is
      W_Dir : aliased C.winnt.WCHAR_array (0 .. Directory'Length);
   begin
      System.Zero_Terminated_WStrings.Convert (Directory, W_Dir (0)'Access);
      if C.winbase.SetCurrentDirectory (W_Dir (0)'Access) = 0 then
         case C.winbase.GetLastError is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_PATH_NOT_FOUND =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
   end Set_Directory;

   procedure Create_Directory (
      New_Directory : String;
      Form : String)
   is
      pragma Unreferenced (Form);
      W_New_Dir : aliased C.winnt.WCHAR_array (0 .. New_Directory'Length);
   begin
      System.Zero_Terminated_WStrings.Convert (
         New_Directory,
         W_New_Dir (0)'Access);
      if C.winbase.CreateDirectory (W_New_Dir (0)'Access, null) = 0 then
         case C.winbase.GetLastError is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_PATH_NOT_FOUND =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
   end Create_Directory;

   procedure Delete_Directory (Directory : String) is
      W_Dir : aliased C.winnt.WCHAR_array (0 .. Directory'Length);
   begin
      System.Zero_Terminated_WStrings.Convert (Directory, W_Dir (0)'Access);
      if C.winbase.RemoveDirectory (W_Dir (0)'Access) = 0 then
         case C.winbase.GetLastError is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_PATH_NOT_FOUND =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
   end Delete_Directory;

   procedure Delete_File (Name : String) is
      W_Name : aliased C.winnt.WCHAR_array (0 .. Name'Length);
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      if C.winbase.DeleteFile (W_Name (0)'Access) = 0 then
         case C.winbase.GetLastError is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_PATH_NOT_FOUND =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
   end Delete_File;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Form : String;
      Overwrite : Boolean)
   is
      pragma Unreferenced (Form);
      W_Source : aliased C.winnt.WCHAR_array (0 .. Source_Name'Length);
      W_Target : aliased C.winnt.WCHAR_array (0 .. Target_Name'Length);
   begin
      System.Zero_Terminated_WStrings.Convert (
         Source_Name,
         W_Source (0)'Access);
      System.Zero_Terminated_WStrings.Convert (
         Target_Name,
         W_Target (0)'Access);
      if C.winbase.CopyFile (
         W_Source (0)'Access,
         W_Target (0)'Access,
         bFailIfExists => Boolean'Pos (not Overwrite)) = 0
      then
         case C.winbase.GetLastError is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_PATH_NOT_FOUND =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
   end Copy_File;

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean)
   is
      W_Old : aliased C.winnt.WCHAR_array (0 .. Old_Name'Length);
      W_New : aliased C.winnt.WCHAR_array (0 .. New_Name'Length);
      Overwrite_Flag : C.windef.DWORD;
   begin
      System.Zero_Terminated_WStrings.Convert (Old_Name, W_Old (0)'Access);
      System.Zero_Terminated_WStrings.Convert (New_Name, W_New (0)'Access);
      if Overwrite then
         Overwrite_Flag := C.winbase.MOVEFILE_REPLACE_EXISTING;
      else
         Overwrite_Flag := 0;
      end if;
      if C.winbase.MoveFileEx (
         W_Old (0)'Access,
         W_New (0)'Access,
         dwFlags => C.winbase.MOVEFILE_COPY_ALLOWED or Overwrite_Flag) = 0
      then
         case C.winbase.GetLastError is
            when C.winerror.ERROR_FILE_NOT_FOUND
               | C.winerror.ERROR_PATH_NOT_FOUND =>
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
   end Rename;

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean) is
   begin
      raise Program_Error; -- try to create junction point ???
   end Symbolic_Link;

   function Full_Name (Name : String) return String is
      Name_Length : constant C.size_t := Name'Length;
      Buffer_Length : constant C.size_t := Name_Length + C.windef.MAX_PATH;
      W_Name : C.winnt.WCHAR_array (0 .. Name_Length);
      Long : C.winnt.WCHAR_array (0 .. Buffer_Length - 1);
      Long_Last : C.size_t;
      Full : C.winnt.WCHAR_array (0 .. Buffer_Length - 1);
      Full_Last : C.size_t;
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      --  expand short filename to long filename
      Long_Last := C.size_t (C.winbase.GetLongPathName (
         W_Name (0)'Access,
         Long (0)'Access,
         Long'Length));
      if Long_Last = 0 or else Long_Last > Long'Last then
         Long (0 .. Name_Length) := W_Name (0 .. Name_Length);
         Long_Last := Name_Length;
      end if;
      --  expand directories
      Full_Last := C.size_t (C.winbase.GetFullPathName (
         Long (0)'Access,
         Full'Length,
         Full (0)'Access,
         null));
      if Full_Last = 0 or else Full_Last > Full'Last then
         Full (0 .. Long_Last) := Long (0 .. Long_Last);
         Full_Last := Long_Last;
      end if;
      --  drive letter to upper case
      if Full_Last >= 2
         and then Wide_Character'Val (Full (1)) = ':'
         and then Wide_Character'Val (Full (0)) in 'a' .. 'z'
      then
         Full (0) := C.winnt.WCHAR'Val (
            C.winnt.WCHAR'Pos (Full (0))
            - (Wide_Character'Pos ('a') - Wide_Character'Pos ('A')));
      end if;
      return System.Zero_Terminated_WStrings.Value (
         Full (0)'Access,
         C.signed_int (Full_Last));
   end Full_Name;

   function Exists (Name : String) return Boolean is
      W_Name : aliased C.winnt.WCHAR_array (0 .. Name'Length);
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      return C.winbase.GetFileAttributesEx (
         W_Name (0)'Access,
         C.winbase.GetFileExInfoStandard,
         C.windef.LPVOID (Information'Address)) /= 0;
   end Exists;

   procedure Get_Information (
      Name : String;
      Information : not null access Directory_Entry_Information_Type)
   is
      W_Name : aliased C.winnt.WCHAR_array (0 .. Name'Length);
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      if C.winbase.GetFileAttributesEx (
         W_Name (0)'Access,
         C.winbase.GetFileExInfoStandard,
         C.windef.LPVOID (Information.all'Address)) = 0
      then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
   end Get_Information;

   function Kind (Information : Directory_Entry_Information_Type)
      return File_Kind is
   begin
      return File_Kind'Enum_Val (Directory_Searching.File_Kind'Enum_Rep (
         Directory_Searching.To_File_Kind (Information.dwFileAttributes)));
   end Kind;

   function Size (Information : Directory_Entry_Information_Type)
      return File_Size
   is
      U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 0,
         LowPart => Information.nFileSizeLow,
         HighPart => Information.nFileSizeHigh);
   begin
      return File_Size (U.QuadPart);
   end Size;

   function Modification_Time (Information : Directory_Entry_Information_Type)
      return System.Native_Time.Native_Time is
   begin
      return Information.ftLastWriteTime;
   end Modification_Time;

   procedure Set_Modification_Time (
      Name : String;
      Time : System.Native_Time.Native_Time)
   is
      W_Name : aliased C.winnt.WCHAR_array (0 .. Name'Length);
      Information : aliased Directory_Entry_Information_Type;
      Aliased_Time : aliased System.Native_Time.Native_Time := Time;
      Handle : C.winnt.HANDLE;
      Dummy : C.windef.WINBOOL;
      pragma Unreferenced (Dummy);
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      if C.winbase.GetFileAttributesEx (
         W_Name (0)'Access,
         C.winbase.GetFileExInfoStandard,
         C.windef.LPVOID (Information'Address)) = 0
      then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
      Handle := C.winbase.CreateFile (
         W_Name (0)'Access,
         dwDesiredAccess => C.winnt.FILE_WRITE_ATTRIBUTES,
         dwShareMode => C.winnt.FILE_SHARE_READ or C.winnt.FILE_SHARE_WRITE,
         lpSecurityAttributes => null,
         dwCreationDisposition => C.winbase.OPEN_EXISTING,
         dwFlagsAndAttributes => C.winbase.FILE_FLAG_BACKUP_SEMANTICS
            or C.winbase.FILE_FLAG_OPEN_REPARSE_POINT,
         hTemplateFile => C.windef.LPVOID (System.Null_Address));
      if Handle = C.winbase.INVALID_HANDLE_VALUE then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      if C.winbase.SetFileTime (
         Handle,
         Information.ftCreationTime'Access,
         Information.ftLastAccessTime'Access,
         Aliased_Time'Access) = 0
      then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      Dummy := C.winbase.CloseHandle (Handle);
   end Set_Modification_Time;

end Ada.Directories.Inside;
