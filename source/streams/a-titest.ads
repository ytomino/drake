pragma License (Unrestricted);
with Ada.Streams.Stream_IO;
private with Ada.Text_IO.Inside;
package Ada.Text_IO.Text_Streams is

--  type Stream_Access is access all Streams.Root_Stream_Type'Class;
   subtype Stream_Access is Streams.Stream_IO.Stream_Access;

   function Stream (File : File_Type) return Stream_Access;
   pragma Inline (Stream); -- renamed

   --  extended
   procedure Open (
      File : in out File_Type;
      Stream : Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "");
   pragma Inline (Open); -- renamed

private

   function Stream (File : File_Type) return Stream_Access
      renames Inside.Stream;

   procedure Open (
      File : in out File_Type;
      Stream : Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "")
      renames Inside.Open;

end Ada.Text_IO.Text_Streams;
