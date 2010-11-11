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
	pragma Wide_Character_Encoding (UTF8); -- convert to UTF-32
	Ada.Wide_Text_IO.Put_Line ("あいうえお");
	Ada.Wide_Wide_Text_IO.Put_Line ("あいうえお");
end textio;
