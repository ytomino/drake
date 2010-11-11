with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
procedure utfconv is
	S : String := "あいうえお";
	W : Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (S);
	R : String := Ada.Strings.UTF_Encoding.Conversions.Convert (W);
begin
	Ada.Debug.Put (R);
end utfconv;
