pragma License (Unrestricted);
--  implementation unit specialized for Darwin
with C.sys.mount;
package System.Native_Directories.Volumes is
   --  File system information.
   pragma Preelaborate;

   subtype File_Size is Ada.Streams.Stream_Element_Count;

   type File_System is record
      Statistics : aliased C.sys.mount.struct_statfs :=
         (f_bsize => 0, others => <>);
      Case_Sensitive : Boolean;
      Case_Sensitive_Valid : Boolean;
   end record;
   pragma Suppress_Initialization (File_System);

   function Is_Assigned (FS : File_System) return Boolean;
   pragma Inline (Is_Assigned);

   Disable_Controlled : constant Boolean := False; --  [gcc-6] crashes if True

   procedure Get (Name : String; FS : aliased out File_System);

   procedure Finalize (FS : in out File_System) is null;

   function Size (FS : File_System) return File_Size;
   function Free_Space (FS : File_System) return File_Size;

   pragma Inline (Size);
   pragma Inline (Free_Space);

   function Owner (FS : File_System) return String;
   function Format_Name (FS : File_System) return String;
   function Directory (FS : File_System) return String;
   function Device (FS : File_System) return String;

   function Case_Preserving (FS : File_System) return Boolean;
   function Case_Sensitive (FS : aliased in out File_System) return Boolean;

   function Is_HFS (FS : File_System) return Boolean;
   pragma Inline (Is_HFS);

   subtype File_System_Id is C.sys.mount.fsid_t;

   function Identity (FS : File_System) return File_System_Id;
   pragma Inline (Identity);

end System.Native_Directories.Volumes;
