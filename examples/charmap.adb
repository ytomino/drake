with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Wide_Maps.Wide_Constants;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
procedure charmap is
	X : Ada.Strings.Maps.Character_Mapping;
	function M (Item : Wide_Wide_Character) return Wide_Wide_Character is
	begin
		case Item is
			when 'A' => return 'B';
			when 'B' => return 'A';
			when others => return Item;
		end case;
	end M;
begin
	X := Ada.Strings.Maps.To_Mapping ("ABC", "ABC");
	pragma Assert (Ada.Strings.Maps.Is_Identity (X));
	X := Ada.Strings.Maps.To_Mapping ("ABC", "あいう");
	pragma Assert (Ada.Strings.Fixed.Translate ("ABCDE", X) = "あいうDE");
	pragma Assert (Ada.Strings.Fixed.Translate ("ABC", M'Access) = "BAC");
	pragma Debug (Ada.Debug.Put ("OK"));
end charmap;
