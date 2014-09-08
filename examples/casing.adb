with Ada.Containers;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Wide_Equal_Case_Insensitive;
with Ada.Strings.Wide_Wide_Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Wide_Less_Case_Insensitive;
with Ada.Strings.Wide_Wide_Less_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Wide_Hash_Case_Insensitive;
with Ada.Strings.Wide_Wide_Hash_Case_Insensitive;
procedure casing is
	use type Ada.Containers.Hash_Type;
	package AS renames Ada.Strings;
	subtype C is Character;
	Full_Width_Upper_A : constant String := (
		C'Val (16#ef#), C'Val (16#bc#), C'Val (16#a1#));
	Full_Width_Lower_A : constant String := (
		C'Val (16#ef#), C'Val (16#bd#), C'Val (16#81#));
begin
	pragma Assert (AS.Equal_Case_Insensitive ("a", "a"));
	pragma Assert (not AS.Equal_Case_Insensitive ("a", "b"));
	pragma Assert (not AS.Equal_Case_Insensitive ("a", "aa"));
	pragma Assert (not AS.Equal_Case_Insensitive ("aa", "a"));
	pragma Assert (AS.Equal_Case_Insensitive ("a", "A"));
	pragma Assert (AS.Equal_Case_Insensitive ("aA", "Aa"));
	pragma Assert (AS.Equal_Case_Insensitive (Full_Width_Lower_A, Full_Width_Upper_A));
	pragma Assert (AS.Less_Case_Insensitive ("a", "B"));
	pragma Assert (not AS.Less_Case_Insensitive ("b", "A"));
	pragma Assert (AS.Less_Case_Insensitive ("a", "AA"));
	pragma Assert (not AS.Less_Case_Insensitive ("aa", "A"));
	pragma Assert (not AS.Less_Case_Insensitive (Full_Width_Lower_A, Full_Width_Upper_A));
	pragma Assert (AS.Hash_Case_Insensitive ("aAa") = AS.Hash_Case_Insensitive ("AaA"));
	-- illegal sequence
	pragma Assert (AS.Less_Case_Insensitive ("", (1 => C'Val (16#80#))));
	pragma Assert (AS.Less_Case_Insensitive (Full_Width_Upper_A, (1 => C'Val (16#80#))));
	pragma Assert (AS.Less_Case_Insensitive (Full_Width_Upper_A, (C'Val (16#ef#), C'Val (16#bd#))));
	pragma Assert (AS.Equal_Case_Insensitive (C'Val (16#80#) & Full_Width_Lower_A, C'Val (16#80#) & Full_Width_Upper_A));
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end casing;
