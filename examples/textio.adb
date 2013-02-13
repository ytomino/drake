with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Text_Streams;
with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Text_IO.Text_Streams;
with System.IO_Options;
procedure textio is
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
		Ada.Text_IO.Create (File_1, Ada.Text_IO.Out_File, Test_File_Name, "shared=yes"); -- shared lock (default is exclusive lock)
		Ada.Text_IO.Open (File_2, Ada.Text_IO.In_File, Test_File_Name); -- dead lock when File_1 acquired exclusive lock
		Ada.Text_IO.Close (File_2);
		Ada.Text_IO.Delete (File_1);
	end;
	-- test form parameter
	declare
		First : Positive;
		Last : Natural;
		Form_1 : constant String := "abc=def";
		Form_2 : constant String := "abc=def,ghi";
		Form_3 : constant String := "ghi";
		Form_4 : constant String := "ghi,abc=def";
	begin
		System.IO_Options.Form_Parameter (Form_1, "abc", First, Last);
		pragma Assert (Form_1 (First .. Last) = "def");
		System.IO_Options.Form_Parameter (Form_1, "a", First, Last);
		pragma Assert (Form_1 (First .. Last) = "" and then First = 1);
		System.IO_Options.Form_Parameter (Form_1, "c", First, Last);
		pragma Assert (Form_1 (First .. Last) = "" and then First = 1);
		System.IO_Options.Form_Parameter (Form_1, "def", First, Last);
		pragma Assert (Form_1 (First .. Last) = "" and then First = 1);
		System.IO_Options.Form_Parameter (Form_2, "abc", First, Last);
		pragma Assert (Form_2 (First .. Last) = "def");
		System.IO_Options.Form_Parameter (Form_2, "ghi", First, Last);
		pragma Assert (Form_2 (First .. Last) = "" and then First > 1);
		System.IO_Options.Form_Parameter (Form_3, "ghi", First, Last);
		pragma Assert (Form_3 (First .. Last) = "" and then First > 1);
		System.IO_Options.Form_Parameter (Form_4, "ghi", First, Last);
		pragma Assert (Form_4 (First .. Last) = "" and then First > 1);
		System.IO_Options.Form_Parameter (Form_4, "abc", First, Last);
		pragma Assert (Form_4 (First .. Last) = "def");
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end textio;
