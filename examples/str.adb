with Ada.Strings.Functions;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded_Strings;
with System;
-- all strings
with Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Bounded;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Bounded;
with Ada.Strings.Wide_Wide_Unbounded;
procedure str is
begin
	-- operators
	declare
		X : String := "11";
	begin
		pragma Assert (X > "0");
		pragma Assert (X < "2");
		pragma Assert (X > "10");
		pragma Assert (X < "12");
		null;
	end;
	-- search functions
	declare
		abcabc : String := "abcabc";
		Text : String := "the south on south";
		function M (Item : Wide_Wide_Character) return Wide_Wide_Character is
		begin
			case Item is
				when 'a' => return 'b';
				when others => return Item;
			end case;
		end M;
	begin
		pragma Assert (Ada.Strings.Functions.Index_Element ("abc", 'b') = 2);
		pragma Assert (Ada.Strings.Functions.Index_Element ("abc", 'd') = 0);
		pragma Assert (Ada.Strings.Functions.Index_Element (abcabc (3 .. 6), 'b') = 5);
		pragma Assert (Ada.Strings.Fixed.Index ("aaabbb", "bbb") = 4);
		pragma Assert (Ada.Strings.Fixed.Index ("aaabbb", "aaa", Going => Ada.Strings.Backward) = 1);
		pragma Assert (Ada.Strings.Fixed.Index ("aaabbb", "ab", Going => Ada.Strings.Backward, Mapping => M'Access) = 0);
		pragma Assert (Ada.Strings.Fixed.Index ("bcac", "bc", Going => Ada.Strings.Backward, Mapping => M'Access) = 3);
		pragma Assert (Ada.Strings.Fixed.Index (Text (10 .. Text'Last), "south") = 14);
		pragma Assert (Ada.Strings.Fixed.Index (Text, "south", 10, Mapping => Ada.Strings.Maps.Identity) = 14);
		pragma Assert (Ada.Strings.Fixed.Index (Text, "SOUTH", 10, Mapping => Ada.Strings.Maps.Constants.Upper_Case_Map) = 14);
		pragma Assert (Ada.Strings.Fixed.Index (Text, "south", 10, Going => Ada.Strings.Backward, Mapping => Ada.Strings.Maps.Identity) = 5);
		pragma Assert (Ada.Strings.Fixed.Index (Text, "SOUTH", 10, Going => Ada.Strings.Backward, Mapping => Ada.Strings.Maps.Constants.Upper_Case_Map) = 5);
		null;
	end;
	-- fixed
	declare
		T : constant String (10 .. 19) := "0123456789";
		R : String (1 .. 3);
	begin
		Ada.Strings.Fixed.Move ("+", R, Justify => Ada.Strings.Center);
		pragma Assert (R = " + ");
		Ada.Strings.Fixed.Move ("++", R, Justify => Ada.Strings.Center);
		pragma Assert (R = "++ ");
		Ada.Strings.Fixed.Move ("+++", R, Justify => Ada.Strings.Center);
		pragma Assert (R = "+++");
		R := "_/_";
		Ada.Strings.Fixed.Trim (R, Side => Ada.Strings.Both, Blank => '_', Justify => Ada.Strings.Center);
		pragma Assert (R = " / ");
		R := "/__";
		Ada.Strings.Fixed.Trim (R, Side => Ada.Strings.Both, Blank => '_', Justify => Ada.Strings.Center);
		pragma Assert (R = " / ");
		R := "__/";
		Ada.Strings.Fixed.Trim (R, Side => Ada.Strings.Both, Blank => '_', Justify => Ada.Strings.Center);
		pragma Assert (R = " / ");
		R := "123";
		Ada.Strings.Fixed.Replace_Slice (R, 2, 3, "4", Justify => Ada.Strings.Right);
		pragma Assert (R = " 14");
		Ada.Strings.Fixed.Replace_Slice (R, 3, 2, "23", Drop => Ada.Strings.Left);
		pragma Assert (R = "234");
		pragma Assert (Ada.Strings.Fixed.Overwrite (T, 13, "DEF") = "012DEF6789");
		pragma Assert (Ada.Strings.Fixed.Delete (T, 13, 16) = "012789");
		pragma Assert (Ada.Strings.Fixed.Delete (T, 30, 0) = T);
		pragma Assert (Ada.Strings.Fixed.Delete (T, T'First, T'Last) = "");
		pragma Assert (Ada.Strings.Fixed."*" (2, "ABC") = "ABCABC");
		pragma Assert (Ada.Strings.Fixed.Head (T, 5) = "01234");
		pragma Assert (Ada.Strings.Fixed.Head ("###", 5) = "###  ");
		pragma Assert (Ada.Strings.Fixed.Tail (T, 5) = "56789");
		pragma Assert (Ada.Strings.Fixed.Tail ("###", 5) = "  ###");
	end;
	-- bounded
	declare
		package BP is new Ada.Strings.Bounded.Generic_Bounded_Length (10);
		use type BP.Bounded_String;
		T : constant BP.Bounded_String := +"123456789A";
		B : BP.Bounded_String := +"123";
	begin
		BP.Delete (B, 2, 2);
		pragma Assert (B = "13");
		pragma Assert (BP.Delete (T, 4, 7) = "12389A");
		B := T;
		BP.Delete (B, 4, 7);
		pragma Assert (B = "12389A");
		pragma Assert (BP.Replicate (2, "ABCDE", Drop => Ada.Strings.Error) = "ABCDEABCDE");
		pragma Assert (BP.Replicate (4, "ABC", Drop => Ada.Strings.Right) = "ABCABCABCA");
		pragma Assert (BP.Replicate (4, "ABC", Drop => Ada.Strings.Left) = "CABCABCABC");
		B := +"123";
		BP.Head (B, 5);
		pragma Assert (B = "123  ");
		BP.Tail (B, 7);
		pragma Assert (B = "  123  ");
		BP.Head (B, 3);
		pragma Assert (B = "  1");
		BP.Tail (B, 1);
		pragma Assert (B = "1");
		pragma Assert (BP.Head (T, 5) = "12345");
		pragma Assert (BP.Head (+"###", 5) = "###  ");
		pragma Assert (BP.Head (T, 12, Drop => Ada.Strings.Left) = "3456789A  ");
		pragma Assert (BP.Tail (T, 5) = "6789A");
		pragma Assert (BP.Tail (+"###", 5) = "  ###");
		pragma Assert (BP.Tail (T, 12, Drop => Ada.Strings.Right) = "  12345678");
	end;
	-- unbounded
	declare
		use type Ada.Strings.Unbounded.Unbounded_String;
		use type System.Address;
		U, V : aliased Ada.Strings.Unbounded.Unbounded_String;
		A : System.Address;
		package CP is new Ada.Strings.Unbounded_Strings.Generic_Constant (new String'("CONSTANT"));
	begin
		pragma Assert (U = "");
		U := +"B";
		Ada.Debug.Put (Integer'Image (U.Capacity));
		pragma Assert (U > "A");
		pragma Assert (U = "B");
		pragma Assert (U < "C");
		Ada.Strings.Unbounded.Set_Unbounded_String (U, "1234"); -- reserve 4
		Ada.Strings.Unbounded.Append (U, "5"); -- reserve 8 (4 * 2 > 4 + 1)
		A := U.Constant_Reference.Element.all'Address;
		V := U; -- sharing
		Ada.Strings.Unbounded.Append (V, "V"); -- use reserved area
		pragma Assert (V.Constant_Reference.Element.all'Address = A); -- same area
		Ada.Strings.Unbounded.Append (U, "U"); -- reallocating
		pragma Assert (U.Constant_Reference.Element.all'Address /= A); -- other area
		pragma Assert (U = "12345U");
		pragma Assert (V = "12345V");
		pragma Assert (V.Reference.Element.all = "12345V");
		A := V.Constant_Reference.Element.all'Address;
		Ada.Strings.Unbounded.Delete (V, 2, 3);
		pragma Assert (V.Constant_Reference.Element.all'Address = A); -- keep area
		pragma Assert (V.Reference.Element.all = "145V");
		U := CP.Value;
		A := U.Constant_Reference.Element.all'Address;
		V := U; -- sharing
		pragma Assert (U = "CONSTANT");
		Ada.Strings.Unbounded.Append (V, "V"); -- reallocating
		pragma Assert (U = "CONSTANT");
		pragma Assert (V = "CONSTANTV");
		pragma Assert (U.Constant_Reference.Element.all'Address = A);
		pragma Assert (V.Constant_Reference.Element.all'Address /= A);
		Ada.Strings.Unbounded.Unbounded_Slice (U, U, 3, 6); -- both Source and Target
		pragma Assert (U = "NSTA");
		Ada.Strings.Unbounded.Append (U, U); -- both Source and Target
		pragma Assert (U = "NSTANSTA");
		Ada.Strings.Unbounded_Strings.Reserve_Capacity (U, 0);
		Ada.Strings.Unbounded.Append (U, U);
		pragma Assert (U = "NSTANSTANSTANSTA");
		U := CP.Value;
		Ada.Strings.Unbounded.Replace_Element (U, 1, 'c'); -- unique
		pragma Assert (U = "cONSTANT");
		pragma Assert (Ada.Strings.Unbounded.Delete (U, 3, 6) = "cONT");
		pragma Assert (Ada.Strings.Unbounded."*" (2, U) = "cONSTANTcONSTANT");
		pragma Assert (Ada.Strings.Unbounded.Replace_Slice (U, 4, 5, "st") = "cONstANT");
		U := +"123";
		Ada.Strings.Unbounded.Head (U, 5);
		pragma Assert (U = "123  ");
		Ada.Strings.Unbounded.Tail (U, 7);
		pragma Assert (U = "  123  ");
		Ada.Strings.Unbounded.Head (U, 3);
		pragma Assert (U = "  1");
		Ada.Strings.Unbounded.Tail (U, 1);
		pragma Assert (U = "1");
		pragma Assert (Ada.Strings.Unbounded.Head (+"###", 5) = "###  ");
		pragma Assert (Ada.Strings.Unbounded.Tail (+"###", 5) = "  ###");
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end str;
