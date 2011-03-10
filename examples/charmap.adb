with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Wide_Maps.Wide_Constants;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
procedure charmap is
	X : Ada.Strings.Maps.Character_Mapping;
begin
	X := Ada.Strings.Maps.To_Mapping ("ABC", "ABC");
	pragma Assert (Ada.Strings.Maps.Is_Identity (X));
	X := Ada.Strings.Maps.To_Mapping ("ABC", "あいう");
	pragma Assert (Ada.Strings.Fixed.Translate ("ABCDE", X) = "あいうDE");
	pragma Debug (Ada.Debug.Put ("OK"));
end charmap;
