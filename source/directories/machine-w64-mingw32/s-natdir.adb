with Ada.Exception_Identification.From_Here;
with System.Zero_Terminated_WStrings;
with C.winerror;
with C.winnt;
package body System.Native_Directories is
   use Ada.Exception_Identification.From_Here;
   use type Ada.Exception_Identification.Exception_Id;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.winnt.HANDLE; -- C.void_ptr

   --  implementation

   function Current_Directory return String is
      Buffer : C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
      Length : C.windef.DWORD;
   begin
      Length := C.winbase.GetCurrentDirectory (
         C.windef.MAX_PATH,
         Buffer (0)'Access);
      if Length = 0 then
         Raise_Exception (Use_Error'Identity);
      else
         return Zero_Terminated_WStrings.Value (
            Buffer (0)'Access,
            C.size_t (Length));
      end if;
   end Current_Directory;

   procedure Set_Directory (Directory : String) is
      W_Directory : aliased C.winnt.WCHAR_array (
         0 ..
         Directory'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Directory, W_Directory (0)'Access);
      if C.winbase.SetCurrentDirectory (W_Directory (0)'Access) =
         C.windef.FALSE
      then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
   end Set_Directory;

   procedure Create_Directory (New_Directory : String) is
      W_New_Directory : aliased C.winnt.WCHAR_array (
         0 ..
         New_Directory'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (
         New_Directory,
         W_New_Directory (0)'Access);
      if C.winbase.CreateDirectory (W_New_Directory (0)'Access, null) =
         C.windef.FALSE
      then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
   end Create_Directory;

   procedure Delete_Directory (Directory : String) is
      W_Directory : aliased C.winnt.WCHAR_array (
         0 ..
         Directory'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Directory, W_Directory (0)'Access);
      if C.winbase.RemoveDirectory (W_Directory (0)'Access) =
         C.windef.FALSE
      then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
   end Delete_Directory;

   procedure Delete_File (Name : String) is
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      if C.winbase.DeleteFile (W_Name (0)'Access) = C.windef.FALSE then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
   end Delete_File;

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean)
   is
      W_Old : aliased C.winnt.WCHAR_array (
         0 ..
         Old_Name'Length * Zero_Terminated_WStrings.Expanding);
      W_New : aliased C.winnt.WCHAR_array (
         0 ..
         New_Name'Length * Zero_Terminated_WStrings.Expanding);
      Overwrite_Flag : C.windef.DWORD;
   begin
      Zero_Terminated_WStrings.To_C (Old_Name, W_Old (0)'Access);
      Zero_Terminated_WStrings.To_C (New_Name, W_New (0)'Access);
      if Overwrite then
         Overwrite_Flag := C.winbase.MOVEFILE_REPLACE_EXISTING;
      else
         Overwrite_Flag := 0;
      end if;
      if C.winbase.MoveFileEx (
            W_Old (0)'Access,
            W_New (0)'Access,
            dwFlags => C.winbase.MOVEFILE_COPY_ALLOWED or Overwrite_Flag) =
         C.windef.FALSE
      then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
   end Rename;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean)
   is
      W_Source_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Source_Name'Length * Zero_Terminated_WStrings.Expanding);
      W_Target_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Target_Name'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Source_Name, W_Source_Name (0)'Access);
      Zero_Terminated_WStrings.To_C (Target_Name, W_Target_Name (0)'Access);
      if C.winbase.CopyFile (
            W_Source_Name (0)'Access,
            W_Target_Name (0)'Access,
            bFailIfExists => Boolean'Pos (not Overwrite)) =
         C.windef.FALSE
      then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
   end Copy_File;

   procedure Replace_File (
      Source_Name : String;
      Target_Name : String)
   is
      W_Source_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Source_Name'Length * Zero_Terminated_WStrings.Expanding);
      W_Target_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Target_Name'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Source_Name, W_Source_Name (0)'Access);
      Zero_Terminated_WStrings.To_C (Target_Name, W_Target_Name (0)'Access);
      if C.winbase.ReplaceFile (
            W_Source_Name (0)'Access,
            W_Target_Name (0)'Access,
            null,
            0,
            C.windef.LPVOID (Null_Address),
            C.windef.LPVOID (Null_Address)) =
         C.windef.FALSE
      then
         --  Target_Name is not existing.
         if C.winbase.MoveFileEx (
               W_Source_Name (0)'Access,
               W_Target_Name (0)'Access,
               dwFlags => C.winbase.MOVEFILE_REPLACE_EXISTING) =
            C.windef.FALSE
         then
            Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
         end if;
      end if;
   end Replace_File;

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean) is
   begin
      raise Program_Error; -- try to create junction point ???
   end Symbolic_Link;

   function Full_Name (Name : String) return String is
      Name_Length : constant C.size_t := Name'Length;
      W_Name : C.winnt.WCHAR_array (
         0 ..
         Name_Length * Zero_Terminated_WStrings.Expanding);
      Buffer_Length : constant C.size_t := Name_Length + C.windef.MAX_PATH;
      Long : C.winnt.WCHAR_array (0 .. Buffer_Length - 1);
      Long_Last : C.size_t;
      Full : C.winnt.WCHAR_array (0 .. Buffer_Length - 1);
      Full_Last : C.size_t;
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      --  expand short filename to long filename
      Long_Last := C.size_t (
         C.winbase.GetLongPathName (
            W_Name (0)'Access,
            Long (0)'Access,
            Long'Length));
      if Long_Last = 0 or else Long_Last > Long'Last then
         Long (0 .. Name_Length) := W_Name (0 .. Name_Length);
         Long_Last := Name_Length;
      end if;
      --  expand directories
      Full_Last := C.size_t (
         C.winbase.GetFullPathName (
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
      return Zero_Terminated_WStrings.Value (Full (0)'Access, Full_Last);
   end Full_Name;

   function Exists (Name : String) return Boolean is
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
      Information : aliased Directory_Entry_Information_Type;
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      return C.winbase.GetFileAttributesEx (
            W_Name (0)'Access,
            C.winbase.GetFileExInfoStandard,
            C.windef.LPVOID (Information'Address)) /=
         C.windef.FALSE;
   end Exists;

   procedure Get_Information (
      Name : String;
      Information : aliased out Directory_Entry_Information_Type)
   is
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      if C.winbase.GetFileAttributesEx (
            W_Name (0)'Access,
            C.winbase.GetFileExInfoStandard,
            C.windef.LPVOID (Information'Address)) =
         C.windef.FALSE
      then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
   end Get_Information;

   function Kind (Attributes : C.windef.DWORD) return File_Kind is
   begin
      if (Attributes and C.winnt.FILE_ATTRIBUTE_DIRECTORY) /= 0 then
         return Directory;
      elsif (Attributes
         and (
            C.winnt.FILE_ATTRIBUTE_DEVICE
            or C.winnt.FILE_ATTRIBUTE_REPARSE_POINT
            or C.winnt.FILE_ATTRIBUTE_VIRTUAL)) /= 0
      then
         return Special_File;
      else
         return Ordinary_File;
      end if;
   end Kind;

   function Kind (Information : Directory_Entry_Information_Type)
      return File_Kind is
   begin
      return File_Kind'Enum_Val (
         File_Kind'Enum_Rep (Kind (Information.dwFileAttributes)));
   end Kind;

   function Size (Information : Directory_Entry_Information_Type)
      return Ada.Streams.Stream_Element_Count
   is
      U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 0,
         LowPart => Information.nFileSizeLow,
         HighPart => Information.nFileSizeHigh);
   begin
      return Ada.Streams.Stream_Element_Offset (U.QuadPart);
   end Size;

   function Modification_Time (Information : Directory_Entry_Information_Type)
      return Native_Calendar.Native_Time is
   begin
      return Information.ftLastWriteTime;
   end Modification_Time;

   procedure Set_Modification_Time (
      Name : String;
      Time : Native_Calendar.Native_Time)
   is
      Exception_Id : Ada.Exception_Identification.Exception_Id :=
         Ada.Exception_Identification.Null_Id;
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
      Information : aliased Directory_Entry_Information_Type;
      Aliased_Time : aliased Native_Calendar.Native_Time := Time;
      Handle : C.winnt.HANDLE;
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      if C.winbase.GetFileAttributesEx (
            W_Name (0)'Access,
            C.winbase.GetFileExInfoStandard,
            C.windef.LPVOID (Information'Address)) =
         C.windef.FALSE
      then
         Exception_Id := Named_IO_Exception_Id (C.winbase.GetLastError);
      else
         Handle := C.winbase.CreateFile (
            W_Name (0)'Access,
            dwDesiredAccess => C.winnt.FILE_WRITE_ATTRIBUTES,
            dwShareMode => C.winnt.FILE_SHARE_READ or C.winnt.FILE_SHARE_WRITE,
            lpSecurityAttributes => null,
            dwCreationDisposition => C.winbase.OPEN_EXISTING,
            dwFlagsAndAttributes =>
               C.winbase.FILE_FLAG_BACKUP_SEMANTICS
               or C.winbase.FILE_FLAG_OPEN_REPARSE_POINT,
            hTemplateFile => C.windef.LPVOID (Null_Address));
         if Handle = C.winbase.INVALID_HANDLE_VALUE then
            Exception_Id := Named_IO_Exception_Id (C.winbase.GetLastError);
         else
            if C.winbase.SetFileTime (
                  Handle,
                  Information.ftCreationTime'Access,
                  Information.ftLastAccessTime'Access,
                  Aliased_Time'Access) =
               C.windef.FALSE
            then
               Exception_Id := IO_Exception_Id (C.winbase.GetLastError);
            end if;
            if C.winbase.CloseHandle (Handle) = C.windef.FALSE then
               if Exception_Id = Ada.Exception_Identification.Null_Id then
                  Exception_Id := IO_Exception_Id (C.winbase.GetLastError);
               end if;
            end if;
         end if;
      end if;
      if Exception_Id /= Ada.Exception_Identification.Null_Id then
         Raise_Exception (Exception_Id);
      end if;
   end Set_Modification_Time;

   function IO_Exception_Id (Error : C.windef.DWORD)
      return Ada.Exception_Identification.Exception_Id is
   begin
      case Error is
         when C.winerror.ERROR_WRITE_FAULT
            | C.winerror.ERROR_READ_FAULT
            | C.winerror.ERROR_GEN_FAILURE
            | C.winerror.ERROR_IO_DEVICE =>
            return Device_Error'Identity;
         when others =>
            return Use_Error'Identity;
      end case;
   end IO_Exception_Id;

   function Named_IO_Exception_Id (Error : C.windef.DWORD)
      return Ada.Exception_Identification.Exception_Id is
   begin
      case Error is
         when C.winerror.ERROR_FILE_NOT_FOUND
            | C.winerror.ERROR_PATH_NOT_FOUND
            | C.winerror.ERROR_INVALID_NAME =>
            return Name_Error'Identity;
         when others =>
            return IO_Exception_Id (Error);
      end case;
   end Named_IO_Exception_Id;

end System.Native_Directories;
