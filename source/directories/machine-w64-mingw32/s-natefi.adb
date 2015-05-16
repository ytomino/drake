with Ada.Exception_Identification.From_Here;
with System.Directory_Searching;
with System.Zero_Terminated_WStrings;
with C.windef;
with C.winbase;
with C.winnt;
package body System.Native_Temporary_Files is
   use Ada.Exception_Identification.From_Here;
   use type C.size_t;
   use type C.windef.UINT;
   use type C.windef.WINBOOL;

   function Named_IO_Exception_Id (errno : C.windef.DWORD)
      return Ada.Exception_Identification.Exception_Id
      renames Directory_Searching.Named_IO_Exception_Id;

   TMP : aliased constant C.winnt.WCHAR_array (0 .. 3) := (
      C.winnt.WCHAR'Val (Wide_Character'Pos ('T')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('M')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('P')),
      C.winnt.WCHAR'Val (0));

   Prefix : constant C.winnt.WCHAR_array (0 .. 3) := (
      C.winnt.WCHAR'Val (Wide_Character'Pos ('A')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('D')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('A')),
      C.winnt.WCHAR'Val (0));

   --  implementation

   function Temporary_Directory return String is
      Result : C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
      Length : C.windef.DWORD;
   begin
      Length := C.winbase.GetTempPath (Result'Length, Result (0)'Access);
      return Zero_Terminated_WStrings.Value (
         Result (0)'Access,
         C.size_t (Length));
   end Temporary_Directory;

   procedure Set_Temporary_Directory (Name : String) is
      W_Name : C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      if C.winbase.SetEnvironmentVariable (
         TMP (0)'Access,
         W_Name (0)'Access) = 0
      then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Set_Temporary_Directory;

   function Create_Temporary_File (
      Directory : String)
      return String
   is
      W_Directory : C.winnt.WCHAR_array (
         0 ..
         Directory'Length * Zero_Terminated_WStrings.Expanding);
      Result : C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
   begin
      Zero_Terminated_WStrings.To_C (Directory, W_Directory (0)'Access);
      if C.winbase.GetTempFileName (
         W_Directory (0)'Access,
         Prefix (0)'Access,
         0,
         Result (0)'Access) = 0
      then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
      return Zero_Terminated_WStrings.Value (Result (0)'Access);
   end Create_Temporary_File;

   function Create_Temporary_Directory (
      Directory : String)
      return String
   is
      Name : constant String := Create_Temporary_File (Directory);
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      if C.winbase.DeleteFile (W_Name (0)'Access) = 0 then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
      if C.winbase.CreateDirectory (W_Name (0)'Access, null) = 0 then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
      return Name;
   end Create_Temporary_Directory;

end System.Native_Temporary_Files;
