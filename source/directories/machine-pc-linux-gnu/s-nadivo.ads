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
      Format_Name_Offset : C.ptrdiff_t;
      Format_Name_Length : C.size_t;
      Directory_Offset : C.ptrdiff_t;
      Directory_Length : C.size_t;
      Device_Offset : C.ptrdiff_t;
      Device_Length : C.size_t;
      Info : C.char_ptr := null; -- the line of /proc/self/mountinfo
   end record;
   pragma Suppress_Initialization (File_System);

   function Is_Assigned (FS : File_System) return Boolean;
   pragma Inline (Is_Assigned);

   Disable_Controlled : constant Boolean := True;

   procedure Get (Name : String; FS : aliased out File_System);

   procedure Finalize (FS : in out File_System);

   function Size (FS : File_System) return File_Size;
   function Free_Space (FS : File_System) return File_Size;

   function Format_Name (FS : aliased in out File_System) return String;
   function Directory (FS : aliased in out File_System) return String;
   function Device (FS : aliased in out File_System) return String;

   function Case_Preserving (FS : File_System) return Boolean is (True);
   function Case_Sensitive (FS : File_System) return Boolean is (True);

   function Is_HFS (FS : File_System) return Boolean is (False);

   subtype File_System_Id is C.sys.types.fsid_t;

   function Identity (FS : File_System) return File_System_Id;

   --  unimplemented
   function Owner (FS : File_System) return String
      with Import, Convention => Ada, External_Name => "__drake_program_error";

end System.Native_Directories.Volumes;
