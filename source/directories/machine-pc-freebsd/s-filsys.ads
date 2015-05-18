pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD
with Ada.Streams;
with C.sys.mount;
package System.File_Systems is
   --  File system information.
   pragma Preelaborate;

   subtype File_Size is Ada.Streams.Stream_Element_Count;

   subtype File_System is C.sys.mount.struct_statfs;

   procedure Get (Name : String; FS : aliased out File_System);

   function Size (FS : File_System) return File_Size;
   function Free_Space (FS : File_System) return File_Size;

   function Owner (FS : File_System) return String;
   function Format_Name (FS : File_System) return String;
   function Directory (FS : File_System) return String; -- mounted to
   function Device (FS : File_System) return String; -- mouted from

   function Case_Preserving (FS : File_System) return Boolean;
   function Case_Sensitive (FS : File_System) return Boolean;

   pragma Inline (Case_Preserving);
   pragma Inline (Case_Sensitive);

   function Is_HFS (FS : File_System) return Boolean;

   pragma Inline (Is_HFS);

   type Root_File_System is record
      Data : aliased File_System;
   end record;
   pragma Suppress_Initialization (Root_File_System);

   function Reference (Item : Root_File_System)
      return not null access File_System;

end System.File_Systems;
