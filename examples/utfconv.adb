with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
procedure utfconv is
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
	pragma Debug (Ada.Debug.Put (
		Ada.Strings.UTF_Encoding.Encoding_Scheme'Image (
			Ada.Strings.UTF_Encoding.UTF_16_Wide_String_Scheme)));
	pragma Debug (Ada.Debug.Put (
		Ada.Strings.UTF_Encoding.Encoding_Scheme'Image (
			Ada.Strings.UTF_Encoding.UTF_32_Wide_Wide_String_Scheme)));
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end utfconv;
