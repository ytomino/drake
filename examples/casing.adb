with Ada.Containers;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
procedure casing is
	use type Ada.Containers.Hash_Type;
begin
	pragma Assert (Ada.Strings.Equal_Case_Insensitive ("a", "a"));
	pragma Assert (not Ada.Strings.Equal_Case_Insensitive ("a", "b"));
	pragma Assert (not Ada.Strings.Equal_Case_Insensitive ("a", "aa"));
	pragma Assert (not Ada.Strings.Equal_Case_Insensitive ("aa", "a"));
	pragma Assert (Ada.Strings.Equal_Case_Insensitive ("a", "A"));
	pragma Assert (Ada.Strings.Equal_Case_Insensitive ("aA", "Aa"));
	pragma Wide_Character_Encoding (BRACKETS); -- keep UTF-8
	pragma Assert (Ada.Strings.Equal_Case_Insensitive ("ａ", "Ａ")); -- full-width A
	pragma Assert (Ada.Strings.Less_Case_Insensitive ("a", "B"));
	pragma Assert (not Ada.Strings.Less_Case_Insensitive ("b", "A"));
	pragma Assert (Ada.Strings.Less_Case_Insensitive ("a", "AA"));
	pragma Assert (not Ada.Strings.Less_Case_Insensitive ("aa", "A"));
	pragma Assert (not Ada.Strings.Less_Case_Insensitive ("ａ", "Ａ")); -- full-width A
	pragma Assert (Ada.Strings.Hash_Case_Insensitive ("aAa") =
		Ada.Strings.Hash_Case_Insensitive ("AaA"));
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end casing;
