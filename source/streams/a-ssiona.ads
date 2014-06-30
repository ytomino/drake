pragma License (Unrestricted);
--  implementation unit
with Ada.Streams.Naked_Stream_IO;
with System.Native_IO;
package Ada.Streams.Stream_IO.Naked is
   pragma Preelaborate;

   --  handle

   procedure Open (
      File : in out File_Type;
      Handle : System.Native_IO.Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : System.Native_IO.Packed_Form := Naked_Stream_IO.Default_Form;
      To_Close : Boolean := False);

   function Handle (File : File_Type) return System.Native_IO.Handle_Type;
   pragma Inline (Handle);

   --  naked

   function Non_Controlled (File : File_Type)
      return not null access Naked_Stream_IO.Non_Controlled_File_Type;
   pragma Inline (Non_Controlled); -- renamed

private

   function Non_Controlled (File : File_Type)
      return not null access Naked_Stream_IO.Non_Controlled_File_Type
      renames Reference;

end Ada.Streams.Stream_IO.Naked;
