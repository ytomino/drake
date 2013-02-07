pragma License (Unrestricted);
--  extended unit
private with Ada.Finalization;
private with C.windef;
private with C.winnt;
package Ada.Directories.Volumes is
   --  File system information.

   type File_System is private;

   function Where (Name : String) return File_System;

--  function Size (FS : File_System) return File_Size;
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

   type File_System is new Finalization.Controlled with record
      Root_Path : C.winnt.LPWSTR := null;
      Root_Path_Length : C.signed_int := 0;
      FileSystemFlags : aliased C.windef.DWORD;
      FileSystemFlags_Valid : Boolean := False;
      Is_NTFS : Boolean;
      Is_NTFS_Valid : Boolean := False;
   end record;

   overriding procedure Adjust (Object : in out File_System);
   overriding procedure Finalize (Object : in out File_System);

end Ada.Directories.Volumes;
