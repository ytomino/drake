with Ada;
with Interfaces.C.Char_Pointers;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with Interfaces.C.Wide_WStrings;
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
		use type Interfaces.C.size_t;
		type A is array (Positive range <>) of aliased Integer;
		package Ps is new Interfaces.C.Pointers (
			Positive,
			Integer,
			A,
			Default_Terminator => 0);
		AO : aliased A (1 .. 10);
		E1 : not null Ps.Pointer := AO (3)'Access;
		E2 : not null Ps.Pointer := AO (7)'Access;
		ZSTR : aliased Interfaces.C.char_array (0 .. 0) := (0 => Interfaces.C.nul);
		Empty_ZSTR : constant Interfaces.C.char_array := Interfaces.C.Char_Pointers.Value (ZSTR (0)'Access, 0);
	begin
		pragma Assert (Ps."-" (E2, E1) = 4);
		pragma Assert (Empty_ZSTR'First = 1 and then Empty_ZSTR'Last = 0);
		null;
	end;
	-- Interfaces.C.Strings
	declare
		use type Interfaces.C.char_array;
		C_Str : aliased Interfaces.C.char_array (1 .. 5) := "12345";
		C_Sub_Str : Interfaces.C.char_array renames C_Str (3 .. 5);
		p : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("ABC");
	begin
		pragma Assert (Interfaces.C.Strings.Value (p) = String'("ABC"));
		Interfaces.C.Strings.Update (p, 1, Interfaces.C.char_array'("Z"));
		pragma Assert (Interfaces.C.Strings.Value (p) = String'("AZC"));
		pragma Assert (Interfaces.C.Strings.Value (p, 1, Append_Nul => True) = ('A', Interfaces.C.nul));
		Interfaces.C.Strings.Free (p);
		pragma Assert (Interfaces.C.Strings.Value (Interfaces.C.Strings.To_Const_Chars_Ptr (C_Sub_Str'Unrestricted_Access), 3) = String'("345"));
	end;
	-- Interfaces.C.Wide_WStrings
	declare
		use type Interfaces.C.wchar_array;
		C_Str : aliased Interfaces.C.wchar_array (1 .. 5) := Interfaces.C.To_C ("12345", Append_Nul => False);
		C_Sub_Str : Interfaces.C.wchar_array renames C_Str (3 .. 5);
		p : Interfaces.C.Wide_WStrings.chars_ptr := Interfaces.C.Wide_WStrings.New_String ("ABC");
	begin
		pragma Assert (Interfaces.C.Wide_WStrings.Value (p) = Wide_String'("ABC"));
		Interfaces.C.Wide_WStrings.Update (p, 1, Interfaces.C.wchar_array'(0 => Interfaces.C.wchar_t'Val (Character'Pos ('Z'))));
		pragma Assert (Interfaces.C.Wide_WStrings.Value (p) = Wide_String'("AZC"));
		pragma Assert (Interfaces.C.Wide_WStrings.Value (p, 1, Append_Nul => True) = (Interfaces.C.wchar_t'Val (Character'Pos ('A')), Interfaces.C.wide_nul));
		Interfaces.C.Wide_WStrings.Free (p);
		pragma Assert (Interfaces.C.Wide_WStrings.Value (Interfaces.C.Wide_WStrings.To_Const_Chars_Ptr (C_Sub_Str'Unrestricted_Access), 3) = Wide_String'("345"));
		p := Interfaces.C.Wide_WStrings.New_Strcat (
			Interfaces.C.Wide_WStrings.const_chars_ptr_With_Length_array'(
				(C_Str (1)'Unchecked_Access, 2),
				(C_Str (4)'Unchecked_Access, 2)));
		pragma Assert (Interfaces.C.Wide_WStrings.Value (p) = "1245");
		Interfaces.C.Wide_WStrings.Free (p);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end intf;
