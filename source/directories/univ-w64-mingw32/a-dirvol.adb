with Ada.Exceptions;
with System.Address_To_Named_Access_Conversions;
with System.Memory;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.winbase;
with C.winerror;
with C.string;
package body Ada.Directories.Volumes is
   use type File_Size;
   use type System.Storage_Elements.Storage_Offset;
   use type C.signed_int;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.winnt.LPWSTR;
   use type C.winnt.HANDLE;
   use type C.winnt.WCHAR;

   package Conv is new System.Address_To_Named_Access_Conversions (
      C.winnt.WCHAR,
      C.winnt.LPWSTR);

   procedure GetVolumeInformation (
      FS : not null access File_System;
      FileSystemNameBuffer : access C.winnt.WCHAR;
      FileSystemNameSize : C.windef.DWORD);
   procedure GetVolumeInformation (
      FS : not null access File_System;
      FileSystemNameBuffer : access C.winnt.WCHAR;
      FileSystemNameSize : C.windef.DWORD) is
   begin
      if FileSystemNameBuffer /= null
         or else not FS.FileSystemFlags_Valid
      then
         if C.winbase.GetVolumeInformation (
            FS.Root_Path,
            null,
            0,
            null,
            null,
            FS.FileSystemFlags'Access,
            FileSystemNameBuffer,
            FileSystemNameSize) = 0
         then
            Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end if;
         --  save FileSystemFlags
         FS.FileSystemFlags_Valid := True;
         --  save NTFS or not
         if not FS.Is_NTFS_Valid and then FileSystemNameBuffer /= null then
            declare
               FileSystem : C.winnt.WCHAR_array (
                  0 ..
                  C.size_t (FileSystemNameSize) - 1);
               for FileSystem'Address use
                  Conv.To_Address (C.winnt.LPWSTR (FileSystemNameBuffer));
            begin
               FS.Is_NTFS := FileSystem (0) = Wide_Character'Pos ('N')
                  and then FileSystem (1) = Wide_Character'Pos ('T')
                  and then FileSystem (2) = Wide_Character'Pos ('F')
                  and then FileSystem (3) = Wide_Character'Pos ('S')
                  and then FileSystem (4) = C.winnt.WCHAR'Val (0);
            end;
            FS.Is_NTFS_Valid := True;
         end if;
      end if;
   end GetVolumeInformation;

   --  implementation

   function Where (Name : String) return File_System is
      W_Name : aliased C.winnt.WCHAR_array (0 .. Name'Length);
      Root_Path : aliased C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      if C.winbase.GetVolumePathName (
         W_Name (0)'Access,
         Root_Path (0)'Access,
         Root_Path'Length) = 0
      then
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
      end if;
      return Result : File_System do
         Result.Root_Path_Length := C.signed_int (
            C.string.wcslen (Root_Path (0)'Access));
         declare
            Dest : constant System.Address := System.Memory.Allocate (
               System.Storage_Elements.Storage_Count (
                  Result.Root_Path_Length)
                  * (C.winnt.WCHAR'Size / Standard'Storage_Unit));
            Dest_S : C.winnt.WCHAR_array (
               0 ..
               C.size_t (Result.Root_Path_Length));
            for Dest_S'Address use Dest;
         begin
            Dest_S := Root_Path (0 .. C.size_t (Result.Root_Path_Length));
            Result.Root_Path := Conv.To_Pointer (Dest);
         end;
      end return;
   end Where;

--  function Size (FS : File_System) return File_Size is
--    Disk_Geometry : aliased C.winioctl.DISK_GEOMETRY;
--    Device : C.winnt.HANDLE;
--    Returned : aliased C.windef.DWORD;
--    Result : C.windef.WINBOOL;
--    Dummy : C.windef.WINBOOL;
--  begin
--    Device := C.winbase.CreateFile (
--       FS.Root_Path,
--       0,
--       C.winnt.FILE_SHARE_READ or C.winnt.FILE_SHARE_WRITE,
--       null,
--       C.winbase.OPEN_EXISTING,
--       0,
--       C.windef.LPVOID (System.Null_Address));
--    if Device = C.winbase.INVALID_HANDLE_VALUE then
--       Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
--    end if;
--    Result := C.winbase.DeviceIoControl (
--       Device,
--       C.winioctl.IOCTL_DISK_GET_DRIVE_GEOMETRY,
--       C.windef.LPVOID (System.Null_Address),
--       0,
--       Disk_Geometry'Access,
--       C.winioctl.DISK_GEOMETRY'Size / Standard'Storage_Unit,
--       Returned'Access,
--       null);
--    Dummy := C.winbase.CloseHandle (Device);
--    if Result = 0 then
--       Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
--    end if;
--    return File_Size (
--       Disk_Geometry.Cylinders.QuadPart
--          * Disk_Geometry.TracksPerCylinder
--          * Disk_Geometry.SectorsPerTrack
--          * Disk_Geometry.BytesPerSector);
--  end Size;

   function Free_Space (FS : File_System) return File_Size is
      FreeBytesAvailable : aliased C.winnt.ULARGE_INTEGER;
      TotalNumberOfBytes : aliased C.winnt.ULARGE_INTEGER;
   begin
      if C.winbase.GetDiskFreeSpaceEx (
         FS.Root_Path,
         FreeBytesAvailable'Access,
         TotalNumberOfBytes'Access,
         null) = 0
      then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      return File_Size (FreeBytesAvailable.QuadPart);
   end Free_Space;

   function Format_Name (FS : File_System) return String is
      FileSystem : aliased C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
   begin
      GetVolumeInformation (
         FS'Unrestricted_Access,
         FileSystem (0)'Access,
         FileSystem'Length);
      return System.Zero_Terminated_WStrings.Value (FileSystem (0)'Access);
   end Format_Name;

   function Directory (FS : File_System) return String is
   begin
      return System.Zero_Terminated_WStrings.Value (
         FS.Root_Path,
         FS.Root_Path_Length);
   end Directory;

   function Device (FS : File_System) return String is
      VolumeName : aliased C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
   begin
      if C.winbase.GetVolumeNameForVolumeMountPoint (
         FS.Root_Path,
         VolumeName (0)'Access,
         VolumeName'Length) = 0
      then
         case C.winbase.GetLastError is
            when C.winerror.ERROR_PATH_NOT_FOUND =>
               --  is it a network drive ?
               --  should it call WNetGetConnection32 to get the UNC path?
               Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
            when others =>
               Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
         end case;
      end if;
      return System.Zero_Terminated_WStrings.Value (VolumeName (0)'Access);
   end Device;

   function Case_Preserving (FS : File_System) return Boolean is
   begin
      GetVolumeInformation (FS'Unrestricted_Access, null, 0);
      return (FS.FileSystemFlags and C.winbase.FS_CASE_IS_PRESERVED) /= 0;
   end Case_Preserving;

   function Case_Sensitive (FS : File_System) return Boolean is
   begin
      if FS.Is_NTFS_Valid then
         --  GetVolumeInformation reports FS_CASE_SENSITIVE at NTFS
         --    though NTFS is case insensitive in the truth.
         if FS.Is_NTFS then
            return False;
         else
            GetVolumeInformation (FS'Unrestricted_Access, null, 0);
            return (FS.FileSystemFlags and C.winbase.FS_CASE_SENSITIVE) /= 0;
         end if;
      else
         declare
            FileSystem : aliased
               C.winnt.WCHAR_array (0 .. C.windef.MAX_PATH - 1);
         begin
            GetVolumeInformation (
               FS'Unrestricted_Access,
               FileSystem (0)'Access,
               FileSystem'Length);
         end;
         return (FS.FileSystemFlags and C.winbase.FS_CASE_SENSITIVE) /= 0
            and then not FS.Is_NTFS;
      end if;
   end Case_Sensitive;

   overriding procedure Adjust (Object : in out File_System) is
   begin
      if Object.Root_Path /= null then
         declare
            Source : constant System.Address :=
               Conv.To_Address (Object.Root_Path);
         begin
            Object.Root_Path := null;
            declare
               Dest : constant System.Address := System.Memory.Allocate (
                  System.Storage_Elements.Storage_Count (
                     Object.Root_Path_Length)
                     * (C.winnt.WCHAR'Size / Standard'Storage_Unit));
               Source_S : C.winnt.WCHAR_array (
                  0 ..
                  C.size_t (Object.Root_Path_Length));
               for Source_S'Address use Source;
               Dest_S : C.winnt.WCHAR_array (
                  0 ..
                  C.size_t (Object.Root_Path_Length));
               for Dest_S'Address use Dest;
            begin
               Dest_S := Source_S;
               Object.Root_Path := Conv.To_Pointer (Dest);
            end;
         end;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out File_System) is
   begin
      System.Memory.Free (Conv.To_Address (Object.Root_Path));
   end Finalize;

end Ada.Directories.Volumes;
