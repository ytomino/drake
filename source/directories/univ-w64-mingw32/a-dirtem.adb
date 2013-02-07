pragma License (Unrestricted);
with Ada.Exceptions;
with System.Zero_Terminated_WStrings;
with C.windef;
with C.winbase;
with C.winnt;
package body Ada.Directories.Temporary is
   use type C.size_t;
   use type C.windef.UINT;
   use type C.windef.WINBOOL;

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
      return System.Zero_Terminated_WStrings.Value (
         Result (0)'Access,
         C.signed_int (Length));
   end Temporary_Directory;

   procedure Set_Temporary_Directory (Name : String) is
      W_Name : C.winnt.WCHAR_array (0 .. Name'Length);
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      if C.winbase.SetEnvironmentVariable (
         TMP (0)'Access,
         W_Name (0)'Access) = 0
      then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
   end Set_Temporary_Directory;

   function Create_Temporary_File (
      Directory : String := Temporary_Directory)
      return String
   is
      W_Dir : C.winnt.WCHAR_array (0 .. Directory'Length);
      Result : C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
   begin
      System.Zero_Terminated_WStrings.Convert (Directory, W_Dir (0)'Access);
      if C.winbase.GetTempFileName (
         W_Dir (0)'Access,
         Prefix (0)'Access,
         0,
         Result (0)'Access) = 0
      then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      return System.Zero_Terminated_WStrings.Value (Result (0)'Access);
   end Create_Temporary_File;

   function Create_Temporary_Directory (
      Directory : String := Temporary_Directory)
      return String
   is
      Name : constant String := Create_Temporary_File (Directory);
   begin
      Delete_File (Name);
      Create_Directory (Name);
      return Name;
   end Create_Temporary_Directory;

end Ada.Directories.Temporary;
