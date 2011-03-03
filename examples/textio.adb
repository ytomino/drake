with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Text_Streams;
with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Text_IO.Text_Streams;
procedure textio is
begin
	Ada.Text_IO.Put_Line ("Hello, Drake runtime!");
	Ada.Wide_Text_IO.Put_Line ("Hello, Drake runtime!");
	Ada.Wide_Wide_Text_IO.Put_Line ("Hello, Drake runtime!");
	pragma Wide_Character_Encoding (BRACKETS); -- keep UTF-8
	Ada.Text_IO.Put_Line ("あいうえお");
	pragma Wide_Character_Encoding (UTF8); -- convert to UTF-16/32
	Ada.Wide_Text_IO.Put_Line ("あいうえお");
	Ada.Wide_Text_IO.Put_Line (Wide_String'("あいうえお"));
	Ada.Wide_Wide_Text_IO.Put_Line ("あいうえお");
	Ada.Wide_Wide_Text_IO.Put_Line (Wide_Wide_String'("あいうえお"));
	-- check inheritance
	Ada.Wide_Text_IO.Set_Col (
	   Ada.Wide_Text_IO.Current_Output.all,
	   Ada.Wide_Text_IO.Col (Ada.Wide_Text_IO.Current_Output.all));
	Ada.Wide_Wide_Text_IO.Set_Col (
	   Ada.Wide_Wide_Text_IO.Current_Output.all,
	   Ada.Wide_Wide_Text_IO.Col (Ada.Wide_Wide_Text_IO.Current_Output.all));
end textio;
