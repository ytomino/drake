with Ada;
with Interfaces.C.Pointers;
-- all instances of Interfaces.C.Generic_Strings
with Interfaces.C.Strings;
with Interfaces.C.Wide_WStrings;
with Interfaces.C.Wide_Wide_WStrings;
procedure intf is
begin
	-- Import
	declare
		type Unsigned_Long_Long is mod 2 ** Long_Long_Integer'Size;
		type P is access all Character;
		function strtoll (str : String; endptr : access P; base : Integer) return Long_Long_Integer
			with Import, Convention => C;
		function strtoull (str : String; endptr : access P; base : Integer) return Unsigned_Long_Long
			with Import, Convention => C;
		N : aliased Long_Long_Integer := -1;
		U : aliased Unsigned_Long_Long := -1;
	begin
		N := strtoll ("100" & ASCII.NUL, null, 10);
		pragma Assert (N = 100);
		U := strtoull ("100" & ASCII.NUL, null, 10);
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
	-- Interfaces.C
	declare
		use type Interfaces.C.size_t;
		use type Interfaces.C.char_array;
		use type Interfaces.C.wchar_array;
		use type Interfaces.C.char16_array;
		use type Interfaces.C.char32_array;
	begin
		pragma Assert (Interfaces.C.To_C (String'("")) = Interfaces.C.char_array'(0 => Interfaces.C.nul));
		pragma Assert (Interfaces.C.To_Ada (Interfaces.C.char_array'(0 => Interfaces.C.nul)) = String'(""));
		pragma Assert (Interfaces.C.To_C (Wide_String'("")) = Interfaces.C.wchar_array'(0 => Interfaces.C.wide_nul));
		pragma Assert (Interfaces.C.To_Ada (Interfaces.C.wchar_array'(0 => Interfaces.C.wide_nul)) = Wide_String'(""));
		pragma Assert (Interfaces.C.To_C (Wide_String'("")) = Interfaces.C.char16_array'(0 => Interfaces.C.char16_nul));
		pragma Assert (Interfaces.C.To_Ada (Interfaces.C.char16_array'(0 => Interfaces.C.char16_nul)) = Wide_String'(""));
		pragma Assert (Interfaces.C.To_C (Wide_Wide_String'("")) = Interfaces.C.char32_array'(0 => Interfaces.C.char32_nul));
		pragma Assert (Interfaces.C.To_Ada (Interfaces.C.char32_array'(0 => Interfaces.C.char32_nul)) = Wide_Wide_String'(""));
		-- contains nul
		pragma Assert (Interfaces.C.To_Ada (Interfaces.C.char_array'(0 => Interfaces.C.nul), Trim_Nul => False) = String'(1 => Character'Val (0)));
		-- substitute
		if Interfaces.C.wchar_t'Size /= Wide_Character'Size then
			pragma Assert (Interfaces.C.To_Wide_String (Interfaces.C.wchar_array'(16#d800#, Interfaces.C.wide_nul), Substitute => "ILSEQ") = "ILSEQ");
			null;
		else
			pragma Assert (Interfaces.C.To_Wide_Wide_String (Interfaces.C.wchar_array'(16#d800#, Interfaces.C.wide_nul), Substitute => "ILSEQ") = "ILSEQ");
			null;
		end if;
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
		C_Str : aliased Interfaces.C.char_array (1 .. 5) := "12345";
		C_Sub_Str : Interfaces.C.char_array renames C_Str (3 .. 5);
		p : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("ABC");
	begin
		pragma Assert (Interfaces.C.Strings.Value (p) = String'("ABC"));
		Interfaces.C.Strings.Update (p, 1, String'("Z"));
		pragma Assert (Interfaces.C.Strings.Value (p) = String'("AZC"));
		Interfaces.C.Strings.Free (p);
		pragma Assert (Interfaces.C.Strings.Value (Interfaces.C.Strings.To_Const_Chars_Ptr (C_Sub_Str'Unrestricted_Access), 3) = String'("345"));
	end;
	-- Interfaces.C.Wide_WStrings
	declare
		C_Str : aliased Interfaces.C.wchar_array (1 .. 5) := Interfaces.C.To_C ("12345", Append_Nul => False);
		C_Sub_Str : Interfaces.C.wchar_array renames C_Str (3 .. 5);
		p : Interfaces.C.Wide_WStrings.chars_ptr := Interfaces.C.Wide_WStrings.New_String ("ABC");
	begin
		pragma Assert (Interfaces.C.Wide_WStrings.Value (p) = Wide_String'("ABC"));
		Interfaces.C.Wide_WStrings.Update (p, 1, Wide_String'("Z"));
		pragma Assert (Interfaces.C.Wide_WStrings.Value (p) = Wide_String'("AZC"));
		Interfaces.C.Wide_WStrings.Free (p);
		pragma Assert (Interfaces.C.Wide_WStrings.Value (Interfaces.C.Wide_WStrings.To_Const_Chars_Ptr (C_Sub_Str'Unrestricted_Access), 3) = Wide_String'("345"));
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end intf;
