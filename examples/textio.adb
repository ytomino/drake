with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Text_Streams;
with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Text_IO.Text_Streams;
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
		File_1, File_2 : Ada.Text_IO.File_Type;
	begin
		Ada.Text_IO.Open (File_1, Ada.Text_IO.In_File, "textio.adb"); -- shared lock
--		Ada.Text_IO.Open (File_1, Ada.Text_IO.Append_File, "textio.adb"); -- exclusive lock
		Ada.Text_IO.Open (File_2, Ada.Text_IO.In_File, "textio.adb"); -- dead lock when File_1 opened as Append_File
	end;
end textio;
