pragma License (Unrestricted);
--  implementation unit
with Ada.Streams.Stream_IO.Inside.Standard_Files;
package Ada.Text_IO.Inside is

   --  handle of stream

   procedure Open (
      File : in out File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "");

   function Stream (File : File_Type) return Streams.Stream_IO.Stream_Access;
   pragma Inline (Stream);
   function Stream_IO (File : File_Type)
      return not null access Streams.Stream_IO.Inside.Non_Controlled_File_Type;
   pragma Inline (Stream_IO);

   --  non-controlled

   type Text_Type (<>) is limited private;
   type Non_Controlled_File_Type is access all Text_Type;

   procedure Create (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "");
   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "");

   procedure Close (
      File : in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True);
   procedure Delete (File : in out Non_Controlled_File_Type);
   procedure Reset (
      File : not null access Non_Controlled_File_Type;
      Mode : File_Mode);

   function Mode (File : Non_Controlled_File_Type) return File_Mode;
   function Name (File : Non_Controlled_File_Type) return String;
   function Form (File : Non_Controlled_File_Type) return String;

   function Is_Open (File : Non_Controlled_File_Type) return Boolean;
   pragma Inline (Is_Open);

   procedure Flush (File : Non_Controlled_File_Type);

   procedure Set_Line_Length (File : Non_Controlled_File_Type; To : Count);
   procedure Set_Page_Length (File : Non_Controlled_File_Type; To : Count);

   function Line_Length (File : Non_Controlled_File_Type) return Count;
   function Page_Length (File : Non_Controlled_File_Type) return Count;

   procedure New_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive_Count := 1);
   procedure Skip_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive_Count := 1);

   function End_Of_Line (File : Non_Controlled_File_Type) return Boolean;

   procedure New_Page (File : Non_Controlled_File_Type);
   procedure Skip_Page (File : Non_Controlled_File_Type);

   function End_Of_Page (File : Non_Controlled_File_Type) return Boolean;
   function End_Of_File (File : Non_Controlled_File_Type) return Boolean;

   procedure Set_Col (File : Non_Controlled_File_Type; To : Positive_Count);
   procedure Set_Line (File : Non_Controlled_File_Type; To : Positive_Count);

   function Col (File : Non_Controlled_File_Type) return Positive_Count;
   function Line (File : Non_Controlled_File_Type) return Positive_Count;
   function Page (File : Non_Controlled_File_Type) return Positive_Count;

   procedure Get (File : Non_Controlled_File_Type; Item : out Character);
   procedure Put (File : Non_Controlled_File_Type; Item : Character);

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character);

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character;
      Available : out Boolean);

   --  handle of stream for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "");

   function Stream (File : Non_Controlled_File_Type)
      return Streams.Stream_IO.Stream_Access;
   function Stream_IO (File : Non_Controlled_File_Type)
      return not null access Streams.Stream_IO.Inside.Non_Controlled_File_Type;

   --  standard I/O

   Standard_Input : constant Non_Controlled_File_Type;
   Standard_Output : constant Non_Controlled_File_Type;
   Standard_Error : constant Non_Controlled_File_Type;

   --  parsing form parameter

   type Encoding_Type is (
      Locale, -- Is_Terminal = False
      Terminal); -- Is_Terminal = True
   pragma Discard_Names (Encoding_Type);

   type Line_Mark_Type is (LF, CR, CRLF);
   pragma Discard_Names (Line_Mark_Type);

   function Form_Encoding (Form : String) return Encoding_Type;
   function Form_Line_Mark (Form : String) return Line_Mark_Type;

private

   type Dummy_Mark_Type is (None, EOP, EOP_EOF, EOF);
   pragma Discard_Names (Dummy_Mark_Type);

   type Text_Type (
      Name_Length : Natural;
      Form_Length : Natural) is limited
   record
      Stream : Streams.Stream_IO.Stream_Access; -- internal stream
      File : aliased Streams.Stream_IO.Inside.Non_Controlled_File_Type;
      Page : Count := 1;
      Line : Count := 1;
      Col : Count := 1;
      Line_Length : Count := 0;
      Page_Length : Count := 0;
      Buffer_Col : Count := 0;
      Last : Natural := 0;
      Buffer : String (1 .. 6);
      End_Of_File : Boolean := False;
      Dummy_Mark : Dummy_Mark_Type := None;
      Mode : File_Mode;
      Encoding : Encoding_Type;
      Line_Mark : Line_Mark_Type;
      Name : String (1 .. Name_Length);
      Form : String (1 .. Form_Length);
   end record;
   pragma Suppress_Initialization (Text_Type);

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out String; -- 1 .. 6
      Last : out Natural;
      End_Of_Line : out Boolean);
   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out String; -- 1 .. 6
      Last : out Natural;
      Wait : Boolean);

   Standard_Input_Text : aliased Text_Type := (
      Name_Length => 0,
      Form_Length => 0,
      Stream => Streams.Stream_IO.Inside.Stream (
         Streams.Stream_IO.Inside.Standard_Files.Standard_Input),
      File => Streams.Stream_IO.Inside.Standard_Files.Standard_Input,
      Mode => In_File,
      Encoding => Encoding_Type'Val (Boolean'Pos (
         Streams.Stream_IO.Inside.Is_Terminal (0))),
      Line_Mark => LF,
      others => <>);

   Standard_Output_Text : aliased Text_Type := (
      Name_Length => 0,
      Form_Length => 0,
      Stream => Streams.Stream_IO.Inside.Stream (
         Streams.Stream_IO.Inside.Standard_Files.Standard_Output),
      File => Streams.Stream_IO.Inside.Standard_Files.Standard_Output,
      Mode => Out_File,
      Encoding => Encoding_Type'Val (Boolean'Pos (
         Streams.Stream_IO.Inside.Is_Terminal (1))),
      Line_Mark => LF,
      others => <>);

   Standard_Error_Text : aliased Text_Type := (
      Name_Length => 0,
      Form_Length => 0,
      Stream => Streams.Stream_IO.Inside.Stream (
         Streams.Stream_IO.Inside.Standard_Files.Standard_Error),
      File => Streams.Stream_IO.Inside.Standard_Files.Standard_Error,
      Mode => Out_File,
      Encoding => Encoding_Type'Val (Boolean'Pos (
         Streams.Stream_IO.Inside.Is_Terminal (2))),
      Line_Mark => LF,
      others => <>);

   Standard_Input : constant Non_Controlled_File_Type :=
      Standard_Input_Text'Access;
   Standard_Output : constant Non_Controlled_File_Type :=
      Standard_Output_Text'Access;
   Standard_Error : constant Non_Controlled_File_Type :=
      Standard_Error_Text'Access;

end Ada.Text_IO.Inside;
