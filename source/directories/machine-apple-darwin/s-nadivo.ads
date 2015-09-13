pragma License (Unrestricted);
--  implementation unit specialized for Darwin
with C.sys.mount;
package System.Native_Directories.Volumes is
   --  File system information.
   pragma Preelaborate;

   subtype File_Size is Ada.Streams.Stream_Element_Count;

   type Non_Controlled_File_System is record
      Statistics : aliased C.sys.mount.struct_statfs64;
      Case_Sensitive : Boolean;
      Case_Sensitive_Valid : Boolean;
   end record;
   pragma Suppress_Initialization (Non_Controlled_File_System);

   function Is_Assigned (FS : Non_Controlled_File_System) return Boolean;

   procedure Get (
      Name : String;
      FS : aliased out Non_Controlled_File_System);

   function Size (FS : Non_Controlled_File_System) return File_Size;
   function Free_Space (FS : Non_Controlled_File_System) return File_Size;

   function Owner (FS : Non_Controlled_File_System) return String;
   function Format_Name (FS : Non_Controlled_File_System) return String;
   function Directory (FS : Non_Controlled_File_System) return String;
   function Device (FS : Non_Controlled_File_System) return String;

   function Case_Preserving (FS : Non_Controlled_File_System) return Boolean;
   function Case_Sensitive (FS : aliased in out Non_Controlled_File_System)
      return Boolean;

   function Is_HFS (FS : Non_Controlled_File_System) return Boolean;

   subtype File_System_Id is C.sys.mount.fsid_t;

   function Identity (FS : Non_Controlled_File_System) return File_System_Id;

   type File_System is record
      Data : aliased Non_Controlled_File_System := (
         (f_bsize => 0, others => <>),
         others => <>);
   end record;

   function Reference (Item : File_System)
      return not null access Non_Controlled_File_System;

end System.Native_Directories.Volumes;
