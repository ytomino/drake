with Ada.Text_IO.Text_Streams;
package body Ada.Wide_Text_IO.Text_Streams is

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Stream : Stream_Access;
      Name : String := "";
      Form : String) is
   begin
      Text_IO.Text_Streams.Open (
         Text_IO.File_Type (File),
         Mode,
         Stream,
         Name,
         Form);
   end Open;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Stream : Stream_Access;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target;
      SUB : IO_Modes.File_SUB := IO_Modes.Ordinary) is
   begin
      Text_IO.Text_Streams.Open (
         Text_IO.File_Type (File),
         Mode,
         Stream,
         Name,
         Shared => Shared,
         Wait => Wait,
         Overwrite => Overwrite,
         External => External,
         New_Line => New_Line,
         SUB => SUB);
   end Open;

   function Stream (File : File_Type) return Stream_Access is
   begin
      return Stream_Access (
         Text_IO.Text_Streams.Stream (Text_IO.File_Type (File)));
   end Stream;

end Ada.Wide_Text_IO.Text_Streams;
