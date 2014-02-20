with Ada.Exception_Identification.From_Here;
with Ada.Streams.Stream_IO.Inside; -- full view
package body Ada.Streams.Stream_IO is
   use Exception_Identification.From_Here;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String) is
   begin
      Inside.Create (
         Reference (File).all,
         Mode,
         Name => Name,
         Form => Inside.Pack (Form));
   end Create;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True) is
   begin
      Inside.Create (
         Reference (File).all,
         Mode,
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
         Inside.Create (
            Reference (Result).all,
            Mode,
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
      Inside.Open (
         Reference (File).all,
         Mode,
         Name => Name,
         Form => Inside.Pack (Form));
   end Open;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True) is
   begin
      Inside.Open (
         Reference (File).all,
         Mode,
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
         Inside.Open (
            Reference (Result).all,
            Mode,
            Name => Name,
            Form => (Shared, Wait, Overwrite));
      end return;
   end Open;

   procedure Close (File : in out File_Type) is
   begin
      Inside.Close (Reference (File), Raise_On_Error => True);
   end Close;

   procedure Delete (File : in out File_Type) is
   begin
      Inside.Delete (Reference (File));
   end Delete;

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      Inside.Reset (Reference (File), Mode);
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Reset (File, Mode (File));
   end Reset;

   function Mode (File : File_Type) return File_Mode is
   begin
      return Inside.Mode (Reference (File).all);
   end Mode;

   function Name (File : File_Type) return String is
   begin
      return Inside.Name (Reference (File).all);
   end Name;

   function Form (File : File_Type) return String is
      Non_Controlled_File : constant Inside.Non_Controlled_File_Type :=
         Reference (File).all;
      Result : Inside.Form_String;
      Last : Natural;
   begin
      Inside.Unpack (Inside.Form (Non_Controlled_File), Result, Last);
      return Result (Result'First .. Last);
   end Form;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Inside.Is_Open (Reference (File).all);
   end Is_Open;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return Inside.End_Of_File (Reference (File).all);
   end End_Of_File;

   function Stream (File : File_Type) return Stream_Access is
   begin
      return Inside.Stream (Reference (File).all);
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
      Non_Controlled_File : constant Inside.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Inside.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      Inside.Read (Non_Controlled_File, Item, Last);
   end Read;

   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array;
      To : Positive_Count) is
   begin
      Set_Index (File, To);
      Write (File, Item, To);
   end Write;

   procedure Write (
      File : File_Type;
      Item : Stream_Element_Array)
   is
      Non_Controlled_File : constant Inside.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Inside.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      Inside.Write (Non_Controlled_File, Item);
   end Write;

   procedure Set_Index (File : File_Type; To : Positive_Count) is
      Non_Controlled_File : constant Inside.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Inside.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      Inside.Set_Index (Non_Controlled_File, To);
   end Set_Index;

   function Index (File : File_Type) return Positive_Count is
      Non_Controlled_File : constant Inside.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Inside.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      return Inside.Index (Non_Controlled_File);
   end Index;

   function Size (File : File_Type) return Count is
      Non_Controlled_File : constant Inside.Non_Controlled_File_Type :=
         Reference (File).all;
   begin
      if not Inside.Is_Open (Non_Controlled_File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      return Inside.Size (Non_Controlled_File);
   end Size;

   procedure Set_Mode (File : in out File_Type; Mode : File_Mode) is
   begin
      Inside.Set_Mode (Reference (File), Mode);
   end Set_Mode;

   procedure Flush (File : File_Type) is
   begin
      Inside.Flush (Reference (File).all);
   end Flush;

   package body Controlled is

      function Reference (File : File_Type)
         return access Inside.Non_Controlled_File_Type is
      begin
         return Inside.Non_Controlled_File_Type (
            File.Stream)'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out File_Type) is
      begin
         if Inside.Is_Open (Reference (Object).all) then
            Inside.Close (Reference (Object), Raise_On_Error => False);
         end if;
      end Finalize;

   end Controlled;

   package body Dispatchers is

      overriding procedure Read (
         Stream : in out Root_Dispatcher;
         Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset) is
      begin
         Inside.Read (
            Inside.Non_Controlled_File_Type (Stream.File),
            Item,
            Last);
      end Read;

      overriding procedure Write (
         Stream : in out Root_Dispatcher;
         Item : Stream_Element_Array) is
      begin
         Inside.Write (Inside.Non_Controlled_File_Type (Stream.File), Item);
      end Write;

      overriding procedure Read (
         Stream : in out Seekable_Dispatcher;
         Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset) is
      begin
         Inside.Read (
            Inside.Non_Controlled_File_Type (Stream.File),
            Item,
            Last);
      end Read;

      overriding procedure Write (
         Stream : in out Seekable_Dispatcher;
         Item : Stream_Element_Array) is
      begin
         Inside.Write (Inside.Non_Controlled_File_Type (Stream.File), Item);
      end Write;

      overriding procedure Set_Index (
         Stream : in out Seekable_Dispatcher;
         To : Stream_Element_Positive_Count) is
      begin
         Inside.Set_Index (Inside.Non_Controlled_File_Type (Stream.File), To);
      end Set_Index;

      overriding function Index (Stream : Seekable_Dispatcher)
         return Stream_Element_Positive_Count is
      begin
         return Inside.Index (Inside.Non_Controlled_File_Type (Stream.File));
      end Index;

      overriding function Size (Stream : Seekable_Dispatcher)
         return Stream_Element_Count is
      begin
         return Inside.Size (Inside.Non_Controlled_File_Type (Stream.File));
      end Size;

   end Dispatchers;

end Ada.Streams.Stream_IO;
