with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with System.UTF_Conversions;
procedure utfconv is
	use type System.UTF_Conversions.UCS_4;
begin
	declare
		pragma Wide_Character_Encoding (BRACKETS);
		S : String := "あいうえお𐄷";
		W : Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (S);
		pragma Wide_Character_Encoding (UTF8);
		pragma Assert (W = "あいうえお" & Wide_Character'Val (16#d800#) & Wide_Character'Val (16#dd37#));
		WR : String := Ada.Strings.UTF_Encoding.Conversions.Convert (W);
		pragma Wide_Character_Encoding (BRACKETS);
		pragma Assert (WR = "あいうえお𐄷");
		WW : Wide_Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (S);
		pragma Wide_Character_Encoding (UTF8);
		pragma Assert (WW = "あいうえお𐄷");
		WWR : String := Ada.Strings.UTF_Encoding.Conversions.Convert (WW);
		pragma Wide_Character_Encoding (BRACKETS);
		pragma Assert (WWR = "あいうえお𐄷");
		D : Wide_Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (W);
		pragma Wide_Character_Encoding (UTF8);
		pragma Assert (D = "あいうえお𐄷");
		DR : Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (D);
		pragma Assert (DR = "あいうえお" & Wide_Character'Val (16#d800#) & Wide_Character'Val (16#dd37#));
	begin
		-- environment-depended value
		pragma Debug (Ada.Debug.Put (
			Ada.Strings.UTF_Encoding.Encoding_Scheme'Image (
				Ada.Strings.UTF_Encoding.UTF_16_Wide_String_Scheme)));
		pragma Debug (Ada.Debug.Put (
			Ada.Strings.UTF_Encoding.Encoding_Scheme'Image (
				Ada.Strings.UTF_Encoding.UTF_32_Wide_Wide_String_Scheme)));
		null;
	end;
	-- handling invalid UTF-8 sequence
	declare
		procedure Test (Item : Character) is
			S : String := (1 => Item);
			Last : Natural;
			Code : System.UTF_Conversions.UCS_4;
			Error : Boolean;
			D : String (1 .. 6);
		begin
			System.UTF_Conversions.From_UTF_8 (S, Last, Code, Error);
			pragma Assert (Error);
			System.UTF_Conversions.To_UTF_8 (Code, D, Last, Error);
			pragma Assert ((System.UTF_Conversions.UCS_4'(Character'Pos (D (Last))) and 16#3f#) = (System.UTF_Conversions.UCS_4'(Character'Pos (Item)) and 16#3f#));
		end Test;
	begin
		Test (Character'Val (16#80#));
		Test (Character'Val (16#8f#));
		Test (Character'Val (16#fe#));
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end utfconv;
