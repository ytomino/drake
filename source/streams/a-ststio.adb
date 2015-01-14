with Ada.Exception_Identification.From_Here;
package body Ada.Streams.Stream_IO is
   use Exception_Identification.From_Here;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String) is
   begin
      Naked_Stream_IO.Create (
         Reference (File).all,
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
      Overwrite : Boolean := True) is
   begin
      Naked_Stream_IO.Create (
         Reference (File).all,
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
         Naked_Stream_IO.Create (
            Reference (Result).all,
            IO_Modes.File_Mode (Mode),
            Name => Name,
            Form => (Shared, Wait, Overwrite));
      end return;
   end Create;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String) is
   begin
      Naked_Stream_IO.Open (
         Reference (File).all,
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
      Overwrite : Boolean := True) is
   begin
      Naked_Stream_IO.Open (
         Reference (File).all,
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
         Naked_Stream_IO.Open (
            Reference (Result).all,
            IO_Modes.File_Mode (Mode),
            Name => Name,
            Form => (Shared, Wait, Overwrite));
      end return;
   end Open;

   procedure Close (File : in out File_Type) is
   begin
      Naked_Stream_IO.Close (Reference (File).all, Raise_On_Error => True);
   end Close;

   procedure Delete (File : in out File_Type) is
   begin
      Naked_Stream_IO.Delete (Reference (File).all);
   end Delete;

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      Naked_Stream_IO.Reset (Reference (File).all, IO_Modes.File_Mode (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Reset (File, Mode (File));
   end Reset;

   function Mode (File : File_Type) return File_Mode is
   begin
      return File_Mode (Naked_Stream_IO.Mode (Reference (File).all));
   end Mode;

   function Name (File : File_Type) return String is
   begin
      return Naked_Stream_IO.Name (Reference (File).all);
   end Name;

   function Form (File : File_Type) return String is
      Non_Controlled_File : constant
         Naked_Stream_IO.Non_Controlled_File_Type :=
         Reference (File).all;
      Result : Naked_Stream_IO.Form_String;
      Last : Natural;
   begin
      Naked_Stream_IO.Unpack (
         Naked_Stream_IO.Form (Non_Controlled_File),
         Result,
         Last);
      return Result (Result'First .. Last);
   end Form;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Naked_Stream_IO.Is_Open (Reference (File).all);
   end Is_Open;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return Naked_Stream_IO.End_Of_File (Reference (File).all);
   end End_Of_File;

   function Stream (File : File_Type) return Stream_Access is
   begin
      return Naked_Stream_IO.Stream (Reference (File).all);
   end Stream;

   procedure Read (
      File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset;
      From : Positive_Count) is
   begin
      Set_Index (File, From);
      Read (File, Item, Last);
   end Read;

   procedure Read (
      File : File_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Non_Controlled_File : constant
         Naked_Stream_IO.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Naked_Stream_IO.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      Naked_Stream_IO.Read (Non_Controlled_File, Item, Last);
   end Read;

   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array;
      To : Positive_Count) is
   begin
      Set_Index (File, To);
      Write (File, Item);
   end Write;

   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array)
   is
      Non_Controlled_File : constant
         Naked_Stream_IO.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Naked_Stream_IO.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      Naked_Stream_IO.Write (Non_Controlled_File, Item);
   end Write;

   procedure Set_Index (File : File_Type; To : Positive_Count) is
      Non_Controlled_File : constant
         Naked_Stream_IO.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Naked_Stream_IO.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      Naked_Stream_IO.Set_Index (Non_Controlled_File, To);
   end Set_Index;

   function Index (File : File_Type) return Positive_Count is
      Non_Controlled_File : constant
         Naked_Stream_IO.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Naked_Stream_IO.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      return Naked_Stream_IO.Index (Non_Controlled_File);
   end Index;

   function Size (File : File_Type) return Count is
      Non_Controlled_File : constant
         Naked_Stream_IO.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Naked_Stream_IO.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      return Naked_Stream_IO.Size (Non_Controlled_File);
   end Size;

   procedure Set_Mode (File : in out File_Type; Mode : File_Mode) is
   begin
      Naked_Stream_IO.Set_Mode (
         Reference (File).all,
         IO_Modes.File_Mode (Mode));
   end Set_Mode;

   procedure Flush (File : File_Type) is
   begin
      Naked_Stream_IO.Flush (Reference (File).all);
   end Flush;

   package body Controlled is

      function Reference (File : File_Type)
         return not null access Naked_Stream_IO.Non_Controlled_File_Type is
      begin
         return File.Stream'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out File_Type) is
         Non_Controlled_File : constant
            not null access Naked_Stream_IO.Non_Controlled_File_Type :=
            Reference (Object);
      begin
         if Naked_Stream_IO.Is_Open (Non_Controlled_File.all) then
            Naked_Stream_IO.Close (
               Non_Controlled_File.all,
               Raise_On_Error => False);
         end if;
      end Finalize;

   end Controlled;

end Ada.Streams.Stream_IO;
