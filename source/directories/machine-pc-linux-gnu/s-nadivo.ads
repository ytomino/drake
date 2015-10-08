pragma License (Unrestricted);
--  implementation unit specialized for Linux
with C.sys.statfs;
package System.Native_Directories.Volumes is
   --  File system information.
   pragma Preelaborate;

   subtype File_Size is Ada.Streams.Stream_Element_Count;

   subtype Non_Controlled_File_System is C.sys.statfs.struct_statfs;

   function Is_Assigned (FS : Non_Controlled_File_System) return Boolean;

   procedure Get (
      Name : String;
      FS : aliased out Non_Controlled_File_System);

   function Size (FS : Non_Controlled_File_System) return File_Size;
   function Free_Space (FS : Non_Controlled_File_System) return File_Size;

   function Case_Preserving (FS : Non_Controlled_File_System) return Boolean;
   function Case_Sensitive (FS : Non_Controlled_File_System) return Boolean;

   pragma Inline (Case_Preserving);
   pragma Inline (Case_Sensitive);

   function Is_HFS (FS : Non_Controlled_File_System) return Boolean;
   pragma Inline (Is_HFS);

   subtype File_System_Id is C.sys.types.fsid_t;

   function Identity (FS : Non_Controlled_File_System) return File_System_Id;

   --  unimplemented
   function Owner (FS : Non_Controlled_File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Format_Name (FS : Non_Controlled_File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Directory (FS : Non_Controlled_File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Device (FS : Non_Controlled_File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";

   type File_System is record
      Data : aliased Non_Controlled_File_System := (
         f_type => 0,
         others => <>);
   end record;

   function Reference (Item : File_System)
      return not null access Non_Controlled_File_System;

end System.Native_Directories.Volumes;
