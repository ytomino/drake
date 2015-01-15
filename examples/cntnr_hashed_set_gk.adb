with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Limited_Hashed_Sets;
procedure cntnr_hashed_set_gk is
	type T is record
		Key : Character;
		Etc : Integer;
	end record;
	function Hash (Key : Character) return Ada.Containers.Hash_Type is
	begin
		return Character'Pos (Key);
	end Hash;
	function Equivalent_Keys (Left, Right : Character) return Boolean
		renames "=";
	function Key (Element : T) return Character is
	begin
		return Element.Key;
	end Key;
	function Hash (Element : T) return Ada.Containers.Hash_Type is
	begin
		return Hash (Key (Element));
	end Hash;
	function Equivalent_Elements (Left, Right : T) return Boolean is
	begin
		return Equivalent_Keys (Key (Left), Key (Right));
	end Equivalent_Elements;
	package Sets is new Ada.Containers.Hashed_Sets (T, Hash, Equivalent_Elements);
	package Sets_Key is new Sets.Generic_Keys (Character, Key, Hash, Equivalent_Keys);
	package ISets is new Ada.Containers.Indefinite_Hashed_Sets (T, Hash, Equivalent_Elements);
	package ISets_Key is new ISets.Generic_Keys (Character, Key, Hash, Equivalent_Keys);
	package LSets is new Ada.Containers.Limited_Hashed_Sets (T, Hash, Equivalent_Elements);
	package LSets_Key is new LSets.Generic_Keys (Character, Key, Hash, Equivalent_Keys);
begin
	declare
		X : Sets.Set;
	begin
		Sets.Insert (X, (Key => 'A', Etc => 1));
		Sets.Insert (X, (Key => 'B', Etc => 2));
		Sets.Insert (X, (Key => 'C', Etc => 3));
		pragma Assert (Sets_Key.Constant_Reference (X, 'B').Element.Etc = 2);
		Sets_Key.Delete (X, 'B');
		pragma Assert (not Sets_Key.Contains (X, 'B'));
	end;
	declare
		X : ISets.Set;
	begin
		ISets.Insert (X, (Key => 'A', Etc => 1));
		ISets.Insert (X, (Key => 'B', Etc => 2));
		ISets.Insert (X, (Key => 'C', Etc => 3));
		pragma Assert (ISets_Key.Constant_Reference (X, 'B').Element.Etc = 2);
		ISets_Key.Delete (X, 'B');
		pragma Assert (not ISets_Key.Contains (X, 'B'));
	end;
	declare
		X : LSets.Set;
		function New_A return T is
		begin
			return (Key => 'A', Etc => 1);
		end New_A;
		function New_B return T is
		begin
			return (Key => 'B', Etc => 2);
		end New_B;
		function New_C return T is
		begin
			return (Key => 'C', Etc => 3);
		end New_C;
	begin
		LSets.Insert (X, New_A'Access);
		LSets.Insert (X, New_B'Access);
		LSets.Insert (X, New_C'Access);
		pragma Assert (LSets_Key.Constant_Reference (X, 'B').Element.Etc = 2);
		LSets_Key.Delete (X, 'B');
		pragma Assert (not LSets_Key.Contains (X, 'B'));
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end cntnr_hashed_set_gk;
