package body Ada.Streams.Stream_IO.Naked is

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Handle : System.Native_IO.Handle_Type;
      Name : String := "";
      Form : System.Native_IO.Packed_Form := Naked_Stream_IO.Default_Form;
      To_Close : Boolean := False)
   is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Open (
         NC_File,
         IO_Modes.File_Mode (Mode),
         Handle,
         Name => Name,
         Form => Form,
         To_Close => To_Close);
   end Open;

   function Handle (File : File_Type) return System.Native_IO.Handle_Type is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      return Naked_Stream_IO.Handle (NC_File);
   end Handle;

end Ada.Streams.Stream_IO.Naked;
