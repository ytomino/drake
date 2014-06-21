package body Ada.Streams.Stream_IO.Naked is

   procedure Open (
      File : in out File_Type;
      Handle : System.Native_IO.Handle_Type;
      Mode : File_Mode;
      Name : String := "";
      Form : System.Native_IO.Packed_Form := Naked_Stream_IO.Default_Form;
      To_Close : Boolean := False) is
   begin
      Naked_Stream_IO.Open (
         Reference (File).all,
         Handle,
         IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => Form,
         To_Close => To_Close);
   end Open;

   function Handle (File : File_Type) return System.Native_IO.Handle_Type is
   begin
      return Naked_Stream_IO.Handle (Reference (File).all);
   end Handle;

end Ada.Streams.Stream_IO.Naked;
