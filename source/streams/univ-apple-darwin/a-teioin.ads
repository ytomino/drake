pragma License (Unrestricted);
--  implementation unit
with Ada.Streams.Stream_IO.Inside.Standard_Files;
package Ada.Text_IO.Inside is

   --  the parameter Form

   type Packed_Form is record
      Stream_Form : Streams.Stream_IO.Inside.Packed_Form;
      External : IO_Text_Modes.File_External_Encoding;
      New_Line : IO_Text_Modes.File_New_Line;
      SUB : IO_Text_Modes.File_SUB;
   end record;
   pragma Suppress_Initialization (Packed_Form);
   pragma Pack (Packed_Form);
   pragma Compile_Time_Error (Packed_Form'Size /= 4, "not packed");

   Default_Form : constant Packed_Form := (
      Stream_Form => Streams.Stream_IO.Inside.Default_Form,
      External => IO_Text_Modes.Locale,
      New_Line => IO_Text_Modes.By_Target,
      SUB => IO_Text_Modes.Ordinary);

   function Pack (Form : String) return Packed_Form;
   procedure Unpack (
      Form : Packed_Form;
      Result : out Streams.Stream_IO.Inside.Form_String;
      Last : out Natural);

   --  handle of stream

   procedure Open (
      File : in out File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : String);
   procedure Open (
      File : in out File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : Packed_Form := Default_Form);

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
      Form : Packed_Form := Default_Form);
   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : Packed_Form := Default_Form);

   procedure Close (
      File : in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True);
   procedure Delete (File : in out Non_Controlled_File_Type);
   procedure Reset (
      File : not null access Non_Controlled_File_Type;
      Mode : File_Mode);

   function Mode (File : Non_Controlled_File_Type) return File_Mode;
   function Name (File : Non_Controlled_File_Type) return String;
   function Form (File : Non_Controlled_File_Type) return Packed_Form;

   function External (File : Non_Controlled_File_Type)
      return IO_Text_Modes.File_External;
   pragma Inline (External);

   function Is_Open (File : Non_Controlled_File_Type) return Boolean;
   pragma Inline (Is_Open);

   procedure Flush (File : Non_Controlled_File_Type);

   procedure Set_Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : Count);

   procedure Set_Line_Length (File : Non_Controlled_File_Type; To : Count);
   procedure Set_Page_Length (File : Non_Controlled_File_Type; To : Count);

   procedure Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : out Count);

   function Line_Length (File : Non_Controlled_File_Type) return Count;
   pragma Inline (Line_Length);
   function Page_Length (File : Non_Controlled_File_Type) return Count;
   pragma Inline (Page_Length);

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

   procedure Set_Position_Within_Terminal (
      File : Non_Controlled_File_Type;
      Col, Line : Positive_Count);
   procedure Set_Col_Within_Terminal (
      File : Non_Controlled_File_Type;
      To : Positive_Count);

   procedure Set_Col (File : Non_Controlled_File_Type; To : Positive_Count);
   procedure Set_Line (File : Non_Controlled_File_Type; To : Positive_Count);

   procedure Position (
      File : Non_Controlled_File_Type;
      Col, Line : out Positive_Count);

   function Col (File : Non_Controlled_File_Type) return Positive_Count;
   pragma Inline (Col);
   function Line (File : Non_Controlled_File_Type) return Positive_Count;
   pragma Inline (Line);
   function Page (File : Non_Controlled_File_Type) return Positive_Count;
   pragma Inline (Page);

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

   procedure View (
      File : Non_Controlled_File_Type;
      Left, Top : out Positive_Count;
      Right, Bottom : out Count);

   --  handle of stream for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : Packed_Form := Default_Form);

   function Stream (File : Non_Controlled_File_Type)
      return Streams.Stream_IO.Stream_Access;
   function Stream_IO (File : Non_Controlled_File_Type)
      return not null access Streams.Stream_IO.Inside.Non_Controlled_File_Type;

   --  standard I/O

   Standard_Input : constant Non_Controlled_File_Type;
   Standard_Output : constant Non_Controlled_File_Type;
   Standard_Error : constant Non_Controlled_File_Type;

   --  form parameter

   function Form_External (Form : String)
      return IO_Text_Modes.File_External_Encoding;
   function Form_New_Line (Form : String) return IO_Text_Modes.File_New_Line;
   function Form_SUB (Form : String) return IO_Text_Modes.File_SUB;

private

   type Dummy_Mark_Type is (None, EOP, EOP_EOF, EOF);
   pragma Discard_Names (Dummy_Mark_Type);

   type Text_Type (
      Name_Length : Natural) is -- "limited" prevents No_Elaboration_Code
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
      External : IO_Text_Modes.File_External;
      New_Line : IO_Text_Modes.File_New_Line;
      SUB : IO_Text_Modes.File_SUB; -- ASCII.SUB = 16#1A#
      Name : String (1 .. Name_Length);
   end record;
   pragma Suppress_Initialization (Text_Type);

   Standard_Input_Text : aliased Text_Type := (
      Name_Length => 0,
      Stream => null, -- overwrite when initialization
      File => Streams.Stream_IO.Inside.Standard_Files.Standard_Input,
      Page => 1,
      Line => 1,
      Col => 1,
      Line_Length => 0,
      Page_Length => 0,
      Buffer_Col => 0,
      Last => 0,
      Buffer => (others => Character'Val (0)),
      End_Of_File => False,
      Dummy_Mark => None,
      Mode => In_File,
      External => IO_Text_Modes.UTF_8, -- overwrite when initialization
      New_Line => IO_Text_Modes.LF,
      SUB => IO_Text_Modes.Ordinary,
      Name => "");

   Standard_Output_Text : aliased Text_Type := (
      Name_Length => 0,
      Stream => null, -- overwrite when initialization
      File => Streams.Stream_IO.Inside.Standard_Files.Standard_Output,
      Page => 1,
      Line => 1,
      Col => 1,
      Line_Length => 0,
      Page_Length => 0,
      Buffer_Col => 0,
      Last => 0,
      Buffer => (others => Character'Val (0)),
      End_Of_File => False,
      Dummy_Mark => None,
      Mode => Out_File,
      External => IO_Text_Modes.UTF_8, -- overwrite when initialization
      New_Line => IO_Text_Modes.LF,
      SUB => IO_Text_Modes.Ordinary,
      Name => "");

   Standard_Error_Text : aliased Text_Type := (
      Name_Length => 0,
      Stream => null, -- overwrite when initialization
      File => Streams.Stream_IO.Inside.Standard_Files.Standard_Error,
      Page => 1,
      Line => 1,
      Col => 1,
      Line_Length => 0,
      Page_Length => 0,
      Buffer_Col => 0,
      Last => 0,
      Buffer => (others => Character'Val (0)),
      End_Of_File => False,
      Dummy_Mark => None,
      Mode => Out_File,
      External => IO_Text_Modes.UTF_8, -- overwrite when initialization
      New_Line => IO_Text_Modes.LF,
      SUB => IO_Text_Modes.Ordinary,
      Name => "");

   Standard_Input : constant Non_Controlled_File_Type :=
      Standard_Input_Text'Access;
   Standard_Output : constant Non_Controlled_File_Type :=
      Standard_Output_Text'Access;
   Standard_Error : constant Non_Controlled_File_Type :=
      Standard_Error_Text'Access;

   --  for Wide_Text_IO/Wide_Wide_Text_IO

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

end Ada.Text_IO.Inside;
