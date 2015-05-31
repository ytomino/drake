package body Ada.Text_IO.Text_Streams is

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Stream : Stream_Access;
      Name : String := "";
      Form : String) is
   begin
      Naked_Text_IO.Open (
         File => Reference (File).all,
         Mode => IO_Modes.File_Mode (Mode),
         Stream => Stream,
         Name => Name,
         Form => Naked_Text_IO.Pack (Form));
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
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target) is
   begin
      Naked_Text_IO.Open (
         File => Reference (File).all,
         Mode => IO_Modes.File_Mode (Mode),
         Stream => Stream,
         Name => Name,
         Form => ((Shared, Wait, Overwrite), External, New_Line));
   end Open;

   function Stream (
      File : File_Type)
      return Stream_Access is
   begin
      return Naked_Text_IO.Stream (Reference (File).all);
   end Stream;

end Ada.Text_IO.Text_Streams;
