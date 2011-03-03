with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
procedure utfconv is
	S : String := "あいうえお";
	W : Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (S);
	WR : String := Ada.Strings.UTF_Encoding.Conversions.Convert (W);
	WW : Wide_Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (S);
	WWR : String := Ada.Strings.UTF_Encoding.Conversions.Convert (WW);
begin
	pragma Assert (WR = S);
	pragma Assert (WWR = S);
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end utfconv;
