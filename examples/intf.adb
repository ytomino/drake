with Ada;
with Interfaces;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
procedure intf is
begin
	-- pragma Import
	declare
		type Unsigned_Long_Long is mod 2 ** Long_Long_Integer'Size;
		procedure sscanf (a1 : String; a2 : String; a3 : access Long_Long_Integer);
		procedure sscanf (a1 : String; a2 : String; a3 : access Unsigned_Long_Long);
		pragma Import (C, sscanf);
		N : aliased Long_Long_Integer := -1;
		U : aliased Unsigned_Long_Long := -1;
	begin
		sscanf ("100", "%lld", N'Access);
		pragma Assert (N = 100);
		sscanf ("100", "%llu", U'Access);
		pragma Assert (U = 100);
	end;
	-- Interfaces
	declare
		use type Interfaces.Integer_32;
		I : aliased Interfaces.Integer_32 := 10;
	begin
		pragma Assert (Interfaces.sync_sub_and_fetch (I'Access, 1) = 9);
		Interfaces.sync_add_and_fetch (I'Access, 1);
		pragma Assert (Interfaces.sync_bool_compare_and_swap (I'Access, 10, 11));
		pragma Assert (not Interfaces.sync_bool_compare_and_swap (I'Access, 12, 13));
	end;
	-- Interfaces.C.Pointers
	declare
		use type Interfaces.C.ptrdiff_t;
		type A is array (Positive range <>) of aliased Integer;
		package Ps is new Interfaces.C.Pointers (
			Positive,
			Integer,
			A,
			Default_Terminator => 0);
		AO : aliased A (1 .. 10);
		E1 : not null Ps.Pointer := AO (3)'Access;
		E2 : not null Ps.Pointer := AO (7)'Access;
	begin
		pragma Assert (Ps."-" (E2, E1) = 4);
		null;
	end;
	-- Interfaces.C.Strings
	declare
		p : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("ABC");
	begin
		pragma Assert (String'(Interfaces.C.Strings.Value (p)) = "ABC");
		Interfaces.C.Strings.Free (p);
	end;
	Ada.Debug.Put ("OK");
end intf;
