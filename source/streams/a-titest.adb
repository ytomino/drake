package body Ada.Text_IO.Text_Streams is

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Stream : Stream_Access;
      Name : String := "";
      Form : String)
   is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Text_IO.Open (
         File => NC_File,
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
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target)
   is
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Text_IO.Open (
         File => NC_File,
         Mode => IO_Modes.File_Mode (Mode),
         Stream => Stream,
         Name => Name,
         Form => ((Shared, Wait, Overwrite), External, New_Line));
   end Open;

   function Open (
      Mode : File_Mode;
      Stream : Stream_Access;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target)
      return File_Type is
   begin
      return Result : File_Type do
         declare
            NC_Result : Naked_Text_IO.Non_Controlled_File_Type
               renames Controlled.Reference (Result).all;
         begin
            Naked_Text_IO.Open (
               File => NC_Result,
               Mode => IO_Modes.File_Mode (Mode),
               Stream => Stream,
               Name => Name,
               Form => ((Shared, Wait, Overwrite), External, New_Line));
         end;
      end return;
   end Open;

   function Stream (
      File : File_Type)
      return Stream_Access
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      return Naked_Text_IO.Stream (NC_File);
   end Stream;

end Ada.Text_IO.Text_Streams;
