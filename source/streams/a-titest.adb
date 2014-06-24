package body Ada.Text_IO.Text_Streams is

   procedure Open (
      File : in out File_Type;
      Stream : Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "") is
   begin
      Naked_Text_IO.Open (
         File => Reference (File).all,
         Stream => Stream,
         Mode => IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => Naked_Text_IO.Pack (Form));
   end Open;

   function Stream (File : File_Type) return Stream_Access is
   begin
      return Naked_Text_IO.Stream (Reference (File).all);
   end Stream;

end Ada.Text_IO.Text_Streams;
