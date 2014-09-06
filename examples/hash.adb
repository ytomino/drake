with Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Wide_Hash;
with Ada.Strings.Wide_Wide_Hash;
with Ada.Strings.Fixed.Hash;
with Ada.Strings.Wide_Fixed.Wide_Hash;
with Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash;
with Ada.Strings.Bounded.Hash;
with Ada.Strings.Wide_Bounded.Wide_Hash;
with Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
procedure hash is
	use type Ada.Containers.Hash_Type;
	type HA is array (Positive range <>) of Ada.Containers.Hash_Type;
	D : HA := (
		Ada.Strings.Hash ("abcdefg"),
		Ada.Strings.Hash ("ab"),
		Ada.Strings.Hash ("ba"),
		Ada.Strings.Hash ("----------"),
		Ada.Strings.Hash ("-----------"),
		Ada.Strings.Hash ("------------"));
begin
	for I in D'First .. D'Last - 1 loop
		for J in I + 1 .. D'Last loop
			pragma Assert (D (I) /= D (J));
			null;
		end loop;
	end loop;
	pragma Debug (Ada.Debug.Put ("OK"));
end hash;
