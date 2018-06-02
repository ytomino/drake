pragma License (Unrestricted);
with Ada.Streams.Stream_IO;
with Ada.Text_IO.Text_Streams;
package Ada.Wide_Text_IO.Text_Streams is

--  type Stream_Access is access all Streams.Root_Stream_Type'Class;
   subtype Stream_Access is Streams.Stream_IO.Stream_Access;

   function Stream (
      File : File_Type) -- Open_File_Type
      return Stream_Access
      renames Text_IO.Text_Streams.Stream;

end Ada.Wide_Text_IO.Text_Streams;
