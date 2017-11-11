package body Ada.Streams.Stream_IO is

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String)
   is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Create (
         NC_File,
         IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => Naked_Stream_IO.Pack (Form));
   end Create;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True)
   is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Create (
         NC_File,
         IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => (Shared, Wait, Overwrite));
   end Create;

   function Create (
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True)
      return File_Type is
   begin
      return Result : File_Type do
         declare
            NC_Result : Naked_Stream_IO.Non_Controlled_File_Type
               renames Controlled.Reference (Result).all;
         begin
            Naked_Stream_IO.Create (
               NC_Result,
               IO_Modes.File_Mode (Mode),
               Name => Name,
               Form => (Shared, Wait, Overwrite));
         end;
      end return;
   end Create;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String)
   is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Open (
         NC_File,
         IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => Naked_Stream_IO.Pack (Form));
   end Open;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True)
   is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Open (
         NC_File,
         IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => (Shared, Wait, Overwrite));
   end Open;

   function Open (
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True)
      return File_Type is
   begin
      return Result : File_Type do
         declare
            NC_Result : Naked_Stream_IO.Non_Controlled_File_Type
               renames Controlled.Reference (Result).all;
         begin
            Naked_Stream_IO.Open (
               NC_Result,
               IO_Modes.File_Mode (Mode),
               Name => Name,
               Form => (Shared, Wait, Overwrite));
         end;
      end return;
   end Open;

   procedure Close (File : in out File_Type) is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Close (NC_File, Raise_On_Error => True);
   end Close;

   procedure Delete (File : in out File_Type) is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Delete (NC_File);
   end Delete;

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Reset (NC_File, IO_Modes.File_Mode (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Reset (File, Mode (File));
   end Reset;

   function Mode (
      File : File_Type)
      return File_Mode
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      return File_Mode (IO_Modes.File_Mode'(Naked_Stream_IO.Mode (NC_File)));
   end Mode;

   function Name (
      File : File_Type)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      return Naked_Stream_IO.Name (NC_File);
   end Name;

   function Form (
      File : File_Type)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
      Result : Naked_Stream_IO.Form_String;
      Last : Natural;
   begin
      Naked_Stream_IO.Unpack (Naked_Stream_IO.Form (NC_File), Result, Last);
      return Result (Result'First .. Last);
   end Form;

   function Is_Open (File : File_Type) return Boolean is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      return Naked_Stream_IO.Is_Open (NC_File);
   end Is_Open;

   function End_Of_File (
      File : File_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      return Naked_Stream_IO.End_Of_File (NC_File);
   end End_Of_File;

   function Stream (
      File : File_Type)
      return Stream_Access
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      return Naked_Stream_IO.Stream (NC_File);
   end Stream;

   procedure Read (
      File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset;
      From : Positive_Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= Out_File or else raise Mode_Error);
   begin
      Set_Index (File, From);
      Read (File, Item, Last);
   end Read;

   procedure Read (
      File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= Out_File or else raise Mode_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Read (NC_File, Item, Last);
   end Read;

   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array;
      To : Positive_Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
   begin
      Set_Index (File, To);
      Write (File, Item);
   end Write;

   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Write (NC_File, Item);
   end Write;

   procedure Set_Index (
      File : File_Type;
      To : Positive_Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Set_Index (NC_File, To);
   end Set_Index;

   function Index (
      File : File_Type)
      return Positive_Count
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      return Naked_Stream_IO.Index (NC_File);
   end Index;

   function Size (
      File : File_Type)
      return Count
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      return Naked_Stream_IO.Size (NC_File);
   end Size;

   procedure Set_Mode (File : in out File_Type; Mode : File_Mode) is
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Set_Mode (NC_File, IO_Modes.File_Mode (Mode));
   end Set_Mode;

   procedure Flush (
      File : File_Type)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      NC_File : Naked_Stream_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      Naked_Stream_IO.Flush (NC_File);
   end Flush;

   package body Controlled is

      function Reference (File : Stream_IO.File_Type)
         return not null access Naked_Stream_IO.Non_Controlled_File_Type is
      begin
         return File_Type (File).Stream'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out File_Type) is
      begin
         if Naked_Stream_IO.Is_Open (Object.Stream) then
            Naked_Stream_IO.Close (Object.Stream, Raise_On_Error => False);
         end if;
      end Finalize;

   end Controlled;

end Ada.Streams.Stream_IO;
