with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.string;
with C.winerror;
package body System.Native_Directories.Volumes is
   use Ada.Exception_Identification.From_Here;
   use type File_Size;
   use type Storage_Elements.Storage_Offset;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.winnt.LPWSTR;
   use type C.winnt.HANDLE; -- C.void_ptr
   use type C.winnt.WCHAR;

   package LPWSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.WCHAR, C.winnt.LPWSTR);

   procedure GetVolumeInformation (
      FS : aliased in out Non_Controlled_File_System;
      FileSystemNameBuffer : C.winnt.LPWSTR;
      FileSystemNameSize : C.windef.DWORD);
   procedure GetVolumeInformation (
      FS : aliased in out Non_Controlled_File_System;
      FileSystemNameBuffer : C.winnt.LPWSTR;
      FileSystemNameSize : C.windef.DWORD) is
   begin
      if FileSystemNameBuffer /= null or else not FS.Valid then
         if C.winbase.GetVolumeInformation (
               FS.Root_Path,
               null,
               0,
               FS.VolumeSerialNumber'Access,
               null,
               FS.FileSystemFlags'Access,
               FileSystemNameBuffer,
               FileSystemNameSize) =
            C.windef.FALSE
         then
            Raise_Exception (IO_Exception_Id (C.winbase.GetLastError));
         end if;
         FS.Valid := True;
         --  save NTFS or not
         if not FS.Is_NTFS_Valid and then FileSystemNameBuffer /= null then
            declare
               pragma Suppress (Alignment_Check);
               FileSystem_All : C.winnt.WCHAR_array (0 .. 4); -- at least
               for FileSystem_All'Address use
                  LPWSTR_Conv.To_Address (FileSystemNameBuffer);
            begin
               FS.Is_NTFS := FileSystem_All (0) = Wide_Character'Pos ('N')
                  and then FileSystem_All (1) = Wide_Character'Pos ('T')
                  and then FileSystem_All (2) = Wide_Character'Pos ('F')
                  and then FileSystem_All (3) = Wide_Character'Pos ('S')
                  and then FileSystem_All (4) = C.winnt.WCHAR'Val (0);
            end;
            FS.Is_NTFS_Valid := True;
         end if;
      end if;
   end GetVolumeInformation;

   --  implementation

   function Is_Assigned (FS : Non_Controlled_File_System) return Boolean is
   begin
      return FS.Root_Path /= null;
   end Is_Assigned;

   procedure Get (
      Name : String;
      FS : aliased out Non_Controlled_File_System)
   is
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
      Root_Path : aliased C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
      Root_Path_Length : C.size_t;
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      if C.winbase.GetVolumePathName (
            W_Name (0)'Access,
            Root_Path (0)'Access,
            Root_Path'Length) =
         C.windef.FALSE
      then
         Raise_Exception (Named_IO_Exception_Id (C.winbase.GetLastError));
      end if;
      Root_Path_Length := C.string.wcslen (Root_Path (0)'Access);
      declare
         pragma Suppress (Alignment_Check);
         Dest : constant Address :=
            Standard_Allocators.Allocate (
               (Storage_Elements.Storage_Offset (Root_Path_Length) + 1)
               * (C.winnt.WCHAR'Size / Standard'Storage_Unit));
         Dest_All : C.winnt.WCHAR_array (0 .. Root_Path_Length);
         for Dest_All'Address use Dest;
      begin
         FS.Root_Path_Length := Root_Path_Length;
         Dest_All := Root_Path (0 .. Root_Path_Length);
         FS.Root_Path := LPWSTR_Conv.To_Pointer (Dest);
      end;
      FS.Valid := False;
      FS.Is_NTFS_Valid := False;
   end Get;

   function Size (FS : Non_Controlled_File_System) return File_Size is
      FreeBytesAvailable : aliased C.winnt.ULARGE_INTEGER;
      TotalNumberOfBytes : aliased C.winnt.ULARGE_INTEGER;
   begin
      if C.winbase.GetDiskFreeSpaceEx (
            FS.Root_Path,
            FreeBytesAvailable'Access,
            TotalNumberOfBytes'Access,
            null) =
         C.windef.FALSE
      then
         Raise_Exception (IO_Exception_Id (C.winbase.GetLastError));
      end if;
      return File_Size (TotalNumberOfBytes.QuadPart);
   end Size;

   function Free_Space (FS : Non_Controlled_File_System) return File_Size is
      FreeBytesAvailable : aliased C.winnt.ULARGE_INTEGER;
      TotalNumberOfBytes : aliased C.winnt.ULARGE_INTEGER;
   begin
      if C.winbase.GetDiskFreeSpaceEx (
            FS.Root_Path,
            FreeBytesAvailable'Access,
            TotalNumberOfBytes'Access,
            null) =
         C.windef.FALSE
      then
         Raise_Exception (IO_Exception_Id (C.winbase.GetLastError));
      end if;
      return File_Size (FreeBytesAvailable.QuadPart);
   end Free_Space;

   function Format_Name (FS : aliased in out Non_Controlled_File_System)
      return String
   is
      FileSystem : aliased C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
   begin
      GetVolumeInformation (
         FS,
         FileSystem (0)'Unchecked_Access,
         FileSystem'Length);
      return Zero_Terminated_WStrings.Value (FileSystem (0)'Access);
   end Format_Name;

   function Directory (FS : Non_Controlled_File_System) return String is
   begin
      return Zero_Terminated_WStrings.Value (
         FS.Root_Path,
         FS.Root_Path_Length);
   end Directory;

   function Device (FS : Non_Controlled_File_System) return String is
      VolumeName : aliased C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
   begin
      if C.winbase.GetVolumeNameForVolumeMountPoint (
            FS.Root_Path,
            VolumeName (0)'Access,
            VolumeName'Length) =
         C.windef.FALSE
      then
         declare
            Error : constant C.windef.DWORD := C.winbase.GetLastError;
         begin
            case Error is
               when C.winerror.ERROR_PATH_NOT_FOUND =>
                  --  is it a network drive ?
                  --  should it call WNetGetConnection32 to get the UNC path?
                  Raise_Exception (Name_Error'Identity);
               when others =>
                  Raise_Exception (IO_Exception_Id (Error));
            end case;
         end;
      end if;
      return Zero_Terminated_WStrings.Value (VolumeName (0)'Access);
   end Device;

   function Case_Preserving (FS : aliased in out Non_Controlled_File_System)
      return Boolean is
   begin
      GetVolumeInformation (FS, null, 0);
      return (FS.FileSystemFlags and C.winbase.FS_CASE_IS_PRESERVED) /= 0;
   end Case_Preserving;

   function Case_Sensitive (FS : aliased in out Non_Controlled_File_System)
      return Boolean is
   begin
      if FS.Is_NTFS_Valid then
         --  GetVolumeInformation reports FS_CASE_SENSITIVE at NTFS
         --    though NTFS is case insensitive in the truth.
         if FS.Is_NTFS then
            return False;
         else
            GetVolumeInformation (FS, null, 0);
            return (FS.FileSystemFlags and C.winbase.FS_CASE_SENSITIVE) /= 0;
         end if;
      else
         declare
            FileSystem : aliased
               C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
         begin
            GetVolumeInformation (
               FS,
               FileSystem (0)'Unchecked_Access,
               FileSystem'Length);
         end;
         return (FS.FileSystemFlags and C.winbase.FS_CASE_SENSITIVE) /= 0
            and then not FS.Is_NTFS;
      end if;
   end Case_Sensitive;

   function Is_HFS (FS : Non_Controlled_File_System) return Boolean is
      pragma Unreferenced (FS);
   begin
      return False;
   end Is_HFS;

   function Identity (FS : aliased in out Non_Controlled_File_System)
      return File_System_Id is
   begin
      GetVolumeInformation (FS, null, 0);
      return FS.VolumeSerialNumber;
   end Identity;

   package body Controlled is

      function Reference (Object : Volumes.File_System)
         return not null access Non_Controlled_File_System is
      begin
         return File_System (Object).Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out File_System) is
      begin
         Standard_Allocators.Free (
            LPWSTR_Conv.To_Address (Object.Data.Root_Path));
      end Finalize;

   end Controlled;

end System.Native_Directories.Volumes;
