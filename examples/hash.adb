with Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Wide_Hash;
with Ada.Strings.Wide_Wide_Hash;
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
	-- Hash = Wide_Hash = Wide_Wide_Hash
	pragma Assert (Ada.Strings.Hash ("Hash") = Ada.Strings.Wide_Hash ("Hash"));
	pragma Assert (Ada.Strings.Hash ("Hash") = Ada.Strings.Wide_Wide_Hash ("Hash"));
	-- the hash algorithm is MurmurHash3 (seed = 0)
	pragma Assert (Ada.Strings.Hash ("") = 0);
	pragma Assert (Ada.Strings.Hash ("a") = 16#56c1cbd1#); -- 61 00 00 00
	pragma Debug (Ada.Debug.Put ("OK"));
end hash;
