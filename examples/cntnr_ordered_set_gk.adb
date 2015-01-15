with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Limited_Ordered_Sets;
procedure cntnr_ordered_set_gk is
	type T is record
		Key : Character;
		Etc : Integer;
	end record;
	function Key (Element : T) return Character is
	begin
		return Element.Key;
	end Key;
	function "<" (Left, Right : T) return Boolean is
	begin
		return Key (Left) < Key (Right);
	end "<";
	package Sets is new Ada.Containers.Ordered_Sets (T);
	package Sets_Key is new Sets.Generic_Keys (Character, Key);
	package ISets is new Ada.Containers.Indefinite_Ordered_Sets (T);
	package ISets_Key is new ISets.Generic_Keys (Character, Key);
	package LSets is new Ada.Containers.Limited_Ordered_Sets (T);
	package LSets_Key is new LSets.Generic_Keys (Character, Key);
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
		pragma Assert (Sets.Constant_Reference (X, Sets_Key.Floor (X, 'B')).Element.Etc = 1);
		pragma Assert (Sets.Constant_Reference (X, Sets_Key.Ceiling (X, 'B')).Element.Etc = 3);
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
		pragma Assert (ISets.Constant_Reference (X, ISets_Key.Floor (X, 'B')).Element.Etc = 1);
		pragma Assert (ISets.Constant_Reference (X, ISets_Key.Ceiling (X, 'B')).Element.Etc = 3);
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
		pragma Assert (LSets.Constant_Reference (X, LSets_Key.Floor (X, 'B')).Element.Etc = 1);
		pragma Assert (LSets.Constant_Reference (X, LSets_Key.Ceiling (X, 'B')).Element.Etc = 3);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end cntnr_ordered_set_gk;
