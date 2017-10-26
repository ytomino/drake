pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C.winnt;
package System.Native_Directories.Volumes is
   --  File system information.
   pragma Preelaborate;

   subtype File_Size is Ada.Streams.Stream_Element_Count;

   type File_System is record
      Root_Path : C.winnt.LPWSTR := null;
      Root_Path_Length : C.size_t;
      VolumeSerialNumber : aliased C.windef.DWORD;
      FileSystemFlags : aliased C.windef.DWORD;
      Valid : Boolean; -- VolumeSerialNumber and FileSystemFlags
      Is_NTFS : Boolean;
      Is_NTFS_Valid : Boolean;
   end record;
   pragma Suppress_Initialization (File_System);

   function Is_Assigned (FS : File_System) return Boolean;
   pragma Inline (Is_Assigned);

   Disable_Controlled : constant Boolean := False;

   procedure Get (Name : String; FS : aliased out File_System);

   procedure Finalize (FS : in out File_System);

   function Size (FS : File_System) return File_Size;
   function Free_Space (FS : File_System) return File_Size;

   function Format_Name (FS : aliased in out File_System) return String;
   function Directory (FS : File_System) return String;
   function Device (FS : File_System) return String; -- GUID

   function Case_Preserving (FS : aliased in out File_System) return Boolean;
   function Case_Sensitive (FS : aliased in out File_System) return Boolean;

   function Is_HFS (FS : File_System) return Boolean is (False);

   subtype File_System_Id is C.windef.DWORD;

   function Identity (FS : aliased in out File_System) return File_System_Id;

   --  unimplemented
   function Owner (FS : File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";

end System.Native_Directories.Volumes;
