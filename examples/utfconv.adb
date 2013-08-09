with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with System.UTF_Conversions;
procedure utfconv is
	use type System.UTF_Conversions.From_Status_Type;
	use type System.UTF_Conversions.UCS_4;
	subtype C is Character;
	subtype WC is Wide_Character;
	subtype WWC is Wide_Wide_Character;
	Seq : constant String := (
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#82#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#84#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#86#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#88#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#8a#),
		C'Val (16#f0#), C'Val (16#90#), C'Val (16#84#), C'Val (16#b7#));
	Wide_Seq : constant Wide_String := (
		WC'Val (16#3042#),
		WC'Val (16#3044#),
		WC'Val (16#3046#),
		WC'Val (16#3048#),
		WC'Val (16#304a#),
		WC'Val (16#d800#), WC'Val (16#dd37#));
	Wide_Wide_Seq : constant Wide_Wide_String := (
		WWC'Val (16#3042#),
		WWC'Val (16#3044#),
		WWC'Val (16#3046#),
		WWC'Val (16#3048#),
		WWC'Val (16#304a#),
		WWC'Val (16#0001_0137#));
begin
	declare
		S : String := Seq;
		W : Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (S);
		pragma Assert (W = Wide_Seq);
		WR : String := Ada.Strings.UTF_Encoding.Conversions.Convert (W);
		pragma Assert (WR = Seq);
		WW : Wide_Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (S);
		pragma Assert (WW = Wide_Wide_Seq);
		WWR : String := Ada.Strings.UTF_Encoding.Conversions.Convert (WW);
		pragma Assert (WWR = Seq);
		D : Wide_Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (W);
		pragma Assert (D = Wide_Wide_Seq);
		DR : Wide_String := Ada.Strings.UTF_Encoding.Conversions.Convert (D);
		pragma Assert (DR = Wide_Seq);
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
			From_Status : System.UTF_Conversions.From_Status_Type;
			To_Status : System.UTF_Conversions.To_Status_Type;
			D : String (1 .. 6);
		begin
			System.UTF_Conversions.From_UTF_8 (S, Last, Code, From_Status);
			pragma Assert (From_Status = System.UTF_Conversions.Illegal_Sequence);
			System.UTF_Conversions.To_UTF_8 (Code, D, Last, To_Status);
			pragma Assert ((System.UTF_Conversions.UCS_4'(Character'Pos (D (Last))) and 16#3f#)
				= (System.UTF_Conversions.UCS_4'(Character'Pos (Item)) and 16#3f#));
		end Test;
	begin
		Test (Character'Val (16#80#));
		Test (Character'Val (16#8f#));
		Test (Character'Val (16#fe#));
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end utfconv;
