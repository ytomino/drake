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
		pragma Assert (Ada.Strings.Fixed.Index ("abc", 'b') = 2);
		pragma Assert (Ada.Strings.Fixed.Index ("abc", 'd') = 0);
		pragma Assert (Ada.Strings.Fixed.Index (abcabc (3 .. 6), 'b') = 5);
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
	end;
	-- bounded
	declare
		package BP is new Ada.Strings.Bounded.Generic_Bounded_Length (10);
		use type BP.Bounded_String;
		B : BP.Bounded_String := +"123";
	begin
		BP.Delete (B, 2, 2);
		pragma Assert (B = "13");
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
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end str;
