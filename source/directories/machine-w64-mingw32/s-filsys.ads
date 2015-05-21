pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.IO_Exceptions;
with Ada.Streams;
with C.windef;
with C.winnt;
private with Ada.Finalization;
package System.File_Systems is
   --  File system information.
   pragma Preelaborate;

   subtype File_Size is Ada.Streams.Stream_Element_Count;

   type Non_Controlled_File_System is record
      Root_Path : C.winnt.LPWSTR;
      Root_Path_Length : C.size_t;
      FileSystemFlags : aliased C.windef.DWORD;
      FileSystemFlags_Valid : Boolean;
      Is_NTFS : Boolean;
      Is_NTFS_Valid : Boolean;
   end record;
   pragma Suppress_Initialization (Non_Controlled_File_System);

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

   --  unimplemented
   function Owner (FS : Non_Controlled_File_System) return String;
   pragma Import (Ada, Owner, "__drake_program_error");

   package Controlled is

      type File_System is limited private;

      function Reference (Object : File_System)
         return not null access Non_Controlled_File_System;
      pragma Inline (Reference);

   private

      type File_System is
         limited new Ada.Finalization.Limited_Controlled with
      record
         Data : aliased Non_Controlled_File_System := (
            Root_Path => null,
            Root_Path_Length => 0,
            FileSystemFlags => <>,
            FileSystemFlags_Valid => False,
            Is_NTFS => <>,
            Is_NTFS_Valid => False);
      end record;

      overriding procedure Finalize (Object : in out File_System);

   end Controlled;

   type File_System is limited new Controlled.File_System;

   --  exceptions

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;

end System.File_Systems;
