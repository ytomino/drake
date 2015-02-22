pragma License (Unrestricted);
--  implementation unit
with Ada.IO_Exceptions;
with Ada.IO_Modes;
with Ada.Streams.Naked_Stream_IO.Standard_Files;
with System.Native_IO;
with System.Native_Text_IO;
package Ada.Naked_Text_IO is

   --  the parameter Form

   Default_Form : constant System.Native_Text_IO.Packed_Form := (
      Stream_Form => Streams.Naked_Stream_IO.Default_Form,
      External => IO_Modes.By_Target,
      New_Line => IO_Modes.By_Target);

   procedure Set (
      Form : in out System.Native_Text_IO.Packed_Form;
      Keyword : String;
      Item : String);
   function Pack (Form : String) return System.Native_Text_IO.Packed_Form;
   procedure Unpack (
      Form : System.Native_Text_IO.Packed_Form;
      Result : out Streams.Naked_Stream_IO.Form_String;
      Last : out Natural);

   --  non-controlled

   type Text_Type (<>) is limited private;
   type Non_Controlled_File_Type is access all Text_Type;

   procedure Create (
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode := IO_Modes.Out_File;
      Name : String := "";
      Form : System.Native_Text_IO.Packed_Form := Default_Form);
   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Name : String;
      Form : System.Native_Text_IO.Packed_Form := Default_Form);

   procedure Close (
      File : aliased in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True);
   procedure Delete (File : aliased in out Non_Controlled_File_Type);
   procedure Reset (
      File : aliased in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode);

   function Mode (File : Non_Controlled_File_Type) return IO_Modes.File_Mode;
   function Name (File : Non_Controlled_File_Type) return String;
   function Form (File : Non_Controlled_File_Type)
      return System.Native_Text_IO.Packed_Form;

   function External (File : Non_Controlled_File_Type)
      return IO_Modes.File_External;
   pragma Inline (External);

   function Is_Open (File : Non_Controlled_File_Type) return Boolean;
   pragma Inline (Is_Open);

   procedure Flush (File : Non_Controlled_File_Type);

   procedure Set_Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : Natural);

   procedure Set_Line_Length (File : Non_Controlled_File_Type; To : Natural);
   procedure Set_Page_Length (File : Non_Controlled_File_Type; To : Natural);

   procedure Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : out Natural);

   function Line_Length (File : Non_Controlled_File_Type) return Natural;
   pragma Inline (Line_Length);
   function Page_Length (File : Non_Controlled_File_Type) return Natural;
   pragma Inline (Page_Length);

   procedure New_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive := 1);
   procedure Skip_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive := 1);

   function End_Of_Line (File : Non_Controlled_File_Type) return Boolean;

   procedure New_Page (File : Non_Controlled_File_Type);
   procedure Skip_Page (File : Non_Controlled_File_Type);

   function End_Of_Page (File : Non_Controlled_File_Type) return Boolean;
   function End_Of_File (File : Non_Controlled_File_Type) return Boolean;

   procedure Set_Col (File : Non_Controlled_File_Type; To : Positive);
   procedure Set_Line (File : Non_Controlled_File_Type; To : Positive);

   procedure Position (
      File : Non_Controlled_File_Type;
      Col, Line : out Positive);

   function Col (File : Non_Controlled_File_Type) return Positive;
   pragma Inline (Col);
   function Line (File : Non_Controlled_File_Type) return Positive;
   pragma Inline (Line);
   function Page (File : Non_Controlled_File_Type) return Positive;
   pragma Inline (Page);

   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Character);
   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character);
   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character);

   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Character);
   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Wide_Character);
   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Wide_Wide_Character);

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Character;
      End_Of_Line : out Boolean);
   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character;
      End_Of_Line : out Boolean);
   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean);

   procedure Skip_Ahead (File : Non_Controlled_File_Type);

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character);
   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character);
   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character);

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character;
      Available : out Boolean);
   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character;
      Available : out Boolean);
   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean);

   --  handle of stream for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Stream : not null access Streams.Root_Stream_Type'Class;
      Name : String := "";
      Form : System.Native_Text_IO.Packed_Form := Default_Form);

   function Stream (File : Non_Controlled_File_Type)
      return not null access Streams.Root_Stream_Type'Class;
   function Stream_IO (File : Non_Controlled_File_Type)
      return not null access
         Streams.Naked_Stream_IO.Non_Controlled_File_Type;

   function Terminal_Handle (File : Non_Controlled_File_Type)
      return System.Native_IO.Handle_Type;

   --  standard I/O

   Standard_Input : constant Non_Controlled_File_Type;
   Standard_Output : constant Non_Controlled_File_Type;
   Standard_Error : constant Non_Controlled_File_Type;

   --  exceptions

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Mode_Error : exception
      renames IO_Exceptions.Mode_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   Device_Error : exception
      renames IO_Exceptions.Device_Error;
   End_Error : exception
      renames IO_Exceptions.End_Error;
   Data_Error : exception
      renames IO_Exceptions.Data_Error;
   Layout_Error : exception
      renames IO_Exceptions.Layout_Error;

private

   type Dummy_Mark_Type is (None, EOP, EOP_EOF, EOF);
   pragma Discard_Names (Dummy_Mark_Type);

   type Text_Type (
      Name_Length : Natural) is -- "limited" prevents No_Elaboration_Code
   record
      Stream : System.Address := -- access Streams.Root_Stream_Type'Class
         System.Null_Address;
      File : aliased Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Page : Natural := 1;
      Line : Natural := 1;
      Col : Natural := 1;
      Line_Length : Natural := 0;
      Page_Length : Natural := 0;
      Last : Natural := 0;
      Ahead_Last : Natural := 0; -- one-character Length, In_Mode only
      Ahead_Col : Natural := 0; -- one-character Col
      Looked_Ahead_Last : Natural := 0;
      Looked_Ahead_Second : String (1 .. 3); -- second of surrogate pair
      Buffer : System.Native_Text_IO.Buffer_Type;
      End_Of_File : Boolean := False;
      Dummy_Mark : Dummy_Mark_Type := None;
      Mode : IO_Modes.File_Mode;
      External : IO_Modes.File_External;
      New_Line : IO_Modes.File_New_Line;
      Name : String (1 .. Name_Length);
   end record;
   pragma Suppress_Initialization (Text_Type);

   Standard_Input_Text : aliased Text_Type := (
      Name_Length => 0,
      Stream => System.Null_Address, -- be overwritten
      File => Streams.Naked_Stream_IO.Standard_Files.Standard_Input,
      Page => 1,
      Line => 1,
      Col => 1,
      Line_Length => 0,
      Page_Length => 0,
      Last => 0,
      Ahead_Last => 0,
      Ahead_Col => 0,
      Looked_Ahead_Last => 0,
      Looked_Ahead_Second => (others => Character'Val (0)),
      Buffer => (others => Character'Val (0)),
      End_Of_File => False,
      Dummy_Mark => None,
      Mode => IO_Modes.In_File,
      External => System.Native_Text_IO.Default_External, -- be overwritten
      New_Line => System.Native_Text_IO.Default_New_Line,
      Name => "");

   Standard_Output_Text : aliased Text_Type := (
      Name_Length => 0,
      Stream => System.Null_Address, -- be overwritten
      File => Streams.Naked_Stream_IO.Standard_Files.Standard_Output,
      Page => 1,
      Line => 1,
      Col => 1,
      Line_Length => 0,
      Page_Length => 0,
      Last => 0,
      Ahead_Last => 0,
      Ahead_Col => 0,
      Looked_Ahead_Last => 0,
      Looked_Ahead_Second => (others => Character'Val (0)),
      Buffer => (others => Character'Val (0)),
      End_Of_File => False,
      Dummy_Mark => None,
      Mode => IO_Modes.Out_File,
      External => System.Native_Text_IO.Default_External, -- be overwritten
      New_Line => System.Native_Text_IO.Default_New_Line,
      Name => "");

   Standard_Error_Text : aliased Text_Type := (
      Name_Length => 0,
      Stream => System.Null_Address, -- be overwritten
      File => Streams.Naked_Stream_IO.Standard_Files.Standard_Error,
      Page => 1,
      Line => 1,
      Col => 1,
      Line_Length => 0,
      Page_Length => 0,
      Last => 0,
      Ahead_Last => 0,
      Ahead_Col => 0,
      Looked_Ahead_Last => 0,
      Looked_Ahead_Second => (others => Character'Val (0)),
      Buffer => (others => Character'Val (0)),
      End_Of_File => False,
      Dummy_Mark => None,
      Mode => IO_Modes.Out_File,
      External => System.Native_Text_IO.Default_External, -- be overwritten
      New_Line => System.Native_Text_IO.Default_New_Line,
      Name => "");

   Standard_Input : constant Non_Controlled_File_Type :=
      Standard_Input_Text'Access;
   Standard_Output : constant Non_Controlled_File_Type :=
      Standard_Output_Text'Access;
   Standard_Error : constant Non_Controlled_File_Type :=
      Standard_Error_Text'Access;

end Ada.Naked_Text_IO;
