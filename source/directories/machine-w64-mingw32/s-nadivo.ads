pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C.winnt;
private with Ada.Finalization;
package System.Native_Directories.Volumes is
   --  File system information.
   pragma Preelaborate;

   subtype File_Size is Ada.Streams.Stream_Element_Count;

   type Non_Controlled_File_System is record
      Root_Path : C.winnt.LPWSTR;
      Root_Path_Length : C.size_t;
      VolumeSerialNumber : aliased C.windef.DWORD;
      FileSystemFlags : aliased C.windef.DWORD;
      Valid : Boolean; -- VolumeSerialNumber and FileSystemFlags
      Is_NTFS : Boolean;
      Is_NTFS_Valid : Boolean;
   end record;
   pragma Suppress_Initialization (Non_Controlled_File_System);

   function Is_Assigned (FS : Non_Controlled_File_System) return Boolean;

   procedure Get (
      Name : String;
      FS : aliased out Non_Controlled_File_System);

   function Size (FS : Non_Controlled_File_System) return File_Size;
   function Free_Space (FS : Non_Controlled_File_System) return File_Size;

   function Format_Name (FS : aliased in out Non_Controlled_File_System)
      return String;
   function Directory (FS : Non_Controlled_File_System) return String;
   function Device (FS : Non_Controlled_File_System) return String; -- GUID

   function Case_Preserving (FS : aliased in out Non_Controlled_File_System)
      return Boolean;
   function Case_Sensitive (FS : aliased in out Non_Controlled_File_System)
      return Boolean;

   function Is_HFS (FS : Non_Controlled_File_System) return Boolean;
   pragma Inline (Is_HFS);

   subtype File_System_Id is C.windef.DWORD;

   function Identity (FS : aliased in out Non_Controlled_File_System)
      return File_System_Id;

   --  unimplemented
   function Owner (FS : Non_Controlled_File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";

   type File_System;

   package Controlled is

      type File_System is limited private;

      function Reference (Object : Volumes.File_System)
         return not null access Non_Controlled_File_System;
      pragma Inline (Reference);

   private

      type File_System is
         limited new Ada.Finalization.Limited_Controlled with
      record
         Data : aliased Non_Controlled_File_System := (
            Root_Path => null,
            Root_Path_Length => 0,
            VolumeSerialNumber => <>,
            FileSystemFlags => <>,
            Valid => False,
            Is_NTFS => <>,
            Is_NTFS_Valid => False);
      end record;

      overriding procedure Finalize (Object : in out File_System);

   end Controlled;

   type File_System is limited new Controlled.File_System;

   function Reference (Object : File_System)
      return not null access Non_Controlled_File_System
      renames Controlled.Reference;

end System.Native_Directories.Volumes;
