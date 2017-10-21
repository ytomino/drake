pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD
with C.sys.mount;
package System.Native_Directories.Volumes is
   --  File system information.
   pragma Preelaborate;

   subtype File_Size is Ada.Streams.Stream_Element_Count;

   subtype Non_Controlled_File_System is C.sys.mount.struct_statfs;

   function Is_Assigned (FS : Non_Controlled_File_System) return Boolean;
   pragma Inline (Is_Assigned);

   procedure Get (
      Name : String;
      FS : aliased out Non_Controlled_File_System);

   function Size (FS : Non_Controlled_File_System) return File_Size;
   function Free_Space (FS : Non_Controlled_File_System) return File_Size;

   pragma Inline (Size);
   pragma Inline (Free_Space);

   function Owner (FS : Non_Controlled_File_System) return String;
   function Format_Name (FS : Non_Controlled_File_System) return String;
   function Directory (FS : Non_Controlled_File_System) return String;
   function Device (FS : Non_Controlled_File_System) return String;

   function Case_Preserving (FS : Non_Controlled_File_System) return Boolean is
      (True);
   function Case_Sensitive (FS : Non_Controlled_File_System) return Boolean is
      (True);

   function Is_HFS (FS : Non_Controlled_File_System) return Boolean is (False);

   subtype File_System_Id is C.sys.mount.fsid_t;

   function Identity (FS : Non_Controlled_File_System) return File_System_Id;
   pragma Inline (Identity);

   type File_System is record
      Data : aliased Non_Controlled_File_System :=
         (f_version => 0, others => <>);
   end record;

   function Reference (Item : File_System)
      return not null access Non_Controlled_File_System;
   pragma Inline (Reference);

end System.Native_Directories.Volumes;
