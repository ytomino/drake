pragma License (Unrestricted);
--  extended unit
private with Ada.Finalization;
private with C.windef;
private with C.winnt;
package Ada.Directories.Volumes is
   --  File system information.

   type File_System is private;

   function Where (Name : String) return File_System;

   function Size (FS : File_System) return File_Size;
   function Free_Space (FS : File_System) return File_Size;
   function Format_Name (FS : File_System) return String;
   function Directory (FS : File_System) return String; -- root directory
   function Device (FS : File_System) return String; -- GUID name

   function Case_Preserving (FS : File_System) return Boolean;
   function Case_Sensitive (FS : File_System) return Boolean;

   --  unimplemented
   function Owner (FS : File_System) return String;
   pragma Import (Ada, Owner, "__drake_program_error");

private

   type Non_Controlled_File_System is record
      Root_Path : C.winnt.LPWSTR;
      Root_Path_Length : C.signed_int;
      FileSystemFlags : aliased C.windef.DWORD;
      FileSystemFlags_Valid : Boolean;
      Is_NTFS : Boolean;
      Is_NTFS_Valid : Boolean;
   end record;
   pragma Suppress_Initialization (Non_Controlled_File_System);

   package Controlled is

      type File_System is private;

      function Reference (Object : File_System)
         return not null access Non_Controlled_File_System;
      pragma Inline (Reference);

   private

      type File_System is new Finalization.Controlled with record
         Data : aliased Non_Controlled_File_System := (
            Root_Path => null,
            Root_Path_Length => 0,
            FileSystemFlags => <>,
            FileSystemFlags_Valid => False,
            Is_NTFS => <>,
            Is_NTFS_Valid => False);
      end record;

      overriding procedure Adjust (Object : in out File_System);
      overriding procedure Finalize (Object : in out File_System);

   end Controlled;

   type File_System is new Controlled.File_System;

end Ada.Directories.Volumes;
