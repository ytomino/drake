with Ada.Command_Line;
with Ada.Streams;
with Ada.Text_IO.Text_Streams;
with Ada.Wide_Text_IO;
with Ada.Wide_Wide_Text_IO;
with System.Form_Parameters;
with System.Standard_Allocators;
procedure textio is
	use type Ada.Streams.Stream_Element_Offset;
	subtype C is Character;
	subtype WC is Wide_Character;
	subtype WWC is Wide_Wide_Character;
	AIUEO : constant String := (
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#82#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#84#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#86#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#88#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#8a#));
	Wide_AIUEO : constant Wide_String := (
		WC'Val (16#3042#),
		WC'Val (16#3044#),
		WC'Val (16#3046#),
		WC'Val (16#3048#),
		WC'Val (16#304a#));
	Wide_Wide_AIUEO : constant Wide_Wide_String := (
		WWC'Val (16#3042#),
		WWC'Val (16#3044#),
		WWC'Val (16#3046#),
		WWC'Val (16#3048#),
		WWC'Val (16#304a#));
begin
	Ada.Text_IO.Put_Line ("Hello, Drake runtime!");
	Ada.Wide_Text_IO.Put_Line ("Hello, Drake runtime!");
	Ada.Wide_Wide_Text_IO.Put_Line ("Hello, Drake runtime!");
	Ada.Text_IO.Put_Line (AIUEO);
	Ada.Wide_Text_IO.Put_Line (Wide_AIUEO);
	Ada.Wide_Wide_Text_IO.Put_Line (Wide_Wide_AIUEO);
	-- check inheritance
	Ada.Wide_Text_IO.Set_Col (
		Ada.Wide_Text_IO.Current_Output.all,
		Ada.Wide_Text_IO.Col (Ada.Wide_Text_IO.Current_Output.all));
	Ada.Wide_Wide_Text_IO.Set_Col (
		Ada.Wide_Wide_Text_IO.Current_Output.all,
		Ada.Wide_Wide_Text_IO.Col (Ada.Wide_Wide_Text_IO.Current_Output.all));
	-- check sharing mode
	declare
		Test_File_Name : constant String := Ada.Command_Line.Command_Name & "-test";
		File_1, File_2 : Ada.Text_IO.File_Type;
	begin
		-- prepare the file beforehand because Create always acquires write-lock
		Ada.Text_IO.Create (File_1, Ada.Text_IO.Out_File, Test_File_Name);
		Ada.Text_IO.Close (File_1);
		-- double open
		Ada.Text_IO.Open (File_1, Ada.Text_IO.Out_File, Test_File_Name, "shared=read"); -- shared lock (default is exclusive lock)
		Ada.Text_IO.Open (File_2, Ada.Text_IO.In_File, Test_File_Name, "shared=yes"); -- dead lock when File_1 acquired exclusive lock
		Ada.Text_IO.Close (File_2);
		Ada.Text_IO.Delete (File_1);
	end;
	-- check Append_File
	declare
		Page_Size : constant Ada.Streams.Stream_Element_Positive_Count :=
			Ada.Streams.Stream_Element_Offset (System.Standard_Allocators.Page_Size);
		Test_File_Name : constant String := Ada.Command_Line.Command_Name & "-test";
		File : Ada.Text_IO.File_Type;
	begin
		Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Test_File_Name);
		Ada.Text_IO.Put (File, "ABC");
		Ada.Text_IO.Close (File);
		Ada.Text_IO.Open (File, Ada.Text_IO.Append_File, Test_File_Name);
		pragma Assert (Ada.Streams.Index (Ada.Streams.Seekable_Stream_Type'Class (Ada.Text_IO.Text_Streams.Stream (File).all)) = 4);
		Ada.Text_IO.Put (File, (1 .. Natural (Page_Size) => ' '));
		Ada.Text_IO.Close (File);
		Ada.Text_IO.Open (File, Ada.Text_IO.Append_File, Test_File_Name);
		pragma Assert (Ada.Streams.Index (Ada.Streams.Seekable_Stream_Type'Class (Ada.Text_IO.Text_Streams.Stream (File).all)) = 4 + Page_Size);
		Ada.Text_IO.Put_Line (File, "DEF");
		Ada.Text_IO.Close (File);
		Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Test_File_Name);
		declare
			Line : constant String := Ada.Text_IO.Get_Line (File);
		begin
			pragma Assert (Line = "ABC" & (1 .. Natural (Page_Size) => ' ') & "DEF");
			null;
		end;
		Ada.Text_IO.Delete (File);
	end;
	-- test form parameter
	declare
		Keyword_First : Positive;
		Keyword_Last : Natural;
		Value_First : Positive;
		Value_Last : Natural;
		Last : Natural;
		Form_1 : constant String := "abc=def";
		Form_2 : constant String := "abc=def,ghi";
		Form_3 : constant String := "ghi";
		Form_4 : constant String := "ghi,abc=def";
	begin
		System.Form_Parameters.Get (Form_1, Keyword_First, Keyword_Last, Value_First, Value_Last, Last);
		pragma Assert (Form_1 (Keyword_First .. Keyword_Last) = "abc");
		pragma Assert (Form_1 (Value_First .. Value_Last) = "def");
		pragma Assert (Last = Form_1'Last);
		System.Form_Parameters.Get (Form_2, Keyword_First, Keyword_Last, Value_First, Value_Last, Last);
		pragma Assert (Form_2 (Keyword_First .. Keyword_Last) = "abc");
		pragma Assert (Form_2 (Value_First .. Value_Last) = "def");
		pragma Assert (Form_2 (Last) = ',');
		System.Form_Parameters.Get (Form_2 (Last + 1 .. Form_2'Last), Keyword_First, Keyword_Last, Value_First, Value_Last, Last);
		pragma Assert (Form_2 (Keyword_First .. Keyword_Last) = "ghi");
		pragma Assert (Form_2 (Value_First .. Value_Last) = "");
		pragma Assert (Last = Form_2'Last);
		System.Form_Parameters.Get (Form_3, Keyword_First, Keyword_Last, Value_First, Value_Last, Last);
		pragma Assert (Form_3 (Keyword_First .. Keyword_Last) = "ghi");
		pragma Assert (Form_3 (Value_First .. Value_Last) = "");
		pragma Assert (Last = Form_3'Last);
		System.Form_Parameters.Get (Form_4, Keyword_First, Keyword_Last, Value_First, Value_Last, Last);
		pragma Assert (Form_4 (Keyword_First .. Keyword_Last) = "ghi");
		pragma Assert (Form_4 (Value_First .. Value_Last) = "");
		pragma Assert (Form_4 (Last) = ',');
		System.Form_Parameters.Get (Form_4 (Last + 1 .. Form_4'Last), Keyword_First, Keyword_Last, Value_First, Value_Last, Last);
		pragma Assert (Form_4 (Keyword_First .. Keyword_Last) = "abc");
		pragma Assert (Form_4 (Value_First .. Value_Last) = "def");
		pragma Assert (Last = Form_4'Last);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end textio;
