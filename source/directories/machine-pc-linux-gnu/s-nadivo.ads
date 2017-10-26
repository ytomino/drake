pragma License (Unrestricted);
--  implementation unit specialized for Linux
with C.sys.statfs;
package System.Native_Directories.Volumes is
   --  File system information.
   pragma Preelaborate;

   subtype File_Size is Ada.Streams.Stream_Element_Count;

   type File_System is record
      Statistics : aliased C.sys.statfs.struct_statfs :=
         (f_type => 0, others => <>);
   end record;
   pragma Suppress_Initialization (File_System);

   function Is_Assigned (FS : File_System) return Boolean;
   pragma Inline (Is_Assigned);

   Disable_Controlled : constant Boolean := True;

   procedure Get (Name : String; FS : aliased out File_System);

   procedure Finalize (FS : in out File_System) is null;
   pragma Inline (Finalize); -- [gcc-7] can not skip calling null procedure

   function Size (FS : File_System) return File_Size;
   function Free_Space (FS : File_System) return File_Size;

   function Case_Preserving (FS : File_System) return Boolean is (True);
   function Case_Sensitive (FS : File_System) return Boolean is (True);

   function Is_HFS (FS : File_System) return Boolean is (False);

   subtype File_System_Id is C.sys.types.fsid_t;

   function Identity (FS : File_System) return File_System_Id;

   --  unimplemented
   function Owner (FS : File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Format_Name (FS : File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Directory (FS : File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   function Device (FS : File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";

end System.Native_Directories.Volumes;
