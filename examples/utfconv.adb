with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
procedure utfconv is
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
	-- add or remove BOM
	declare
		package ASUE renames Ada.Strings.UTF_Encoding;
	begin
		pragma Assert (ASUE.Strings.Encode ("A", ASUE.UTF_8, True) = ASUE.UTF_String'(ASUE.BOM_8 & "A"));
		pragma Assert (ASUE.Strings.Encode ("A", True) = ASUE.BOM_8 & "A");
		pragma Assert (ASUE.Strings.Decode (ASUE.UTF_String'(ASUE.BOM_8 & "A"), ASUE.UTF_8) = "A");
		pragma Assert (ASUE.Strings.Decode (ASUE.BOM_8 & "A") = "A");
		pragma Assert (ASUE.Wide_Strings.Encode ("A", ASUE.UTF_16BE, True) = ASUE.UTF_String'(ASUE.BOM_16BE & C'Val (0) & "A"));
		pragma Assert (ASUE.Wide_Strings.Encode ("A", True) = ASUE.BOM_16 & "A");
		pragma Assert (ASUE.Wide_Strings.Decode (ASUE.UTF_String'(ASUE.BOM_16BE & C'Val (0) & "A"), ASUE.UTF_16BE) = "A");
		pragma Assert (ASUE.Wide_Strings.Decode (ASUE.BOM_16 & "A") = "A");
		pragma Assert (ASUE.Wide_Wide_Strings.Encode ("A", ASUE.UTF_32BE, True) = ASUE.UTF_String'(ASUE.BOM_32BE & C'Val (0) & C'Val (0) & C'Val (0) & "A"));
		pragma Assert (ASUE.Wide_Wide_Strings.Encode ("A", True) = ASUE.BOM_32 & "A");
		pragma Assert (ASUE.Wide_Wide_Strings.Decode (ASUE.UTF_String'(ASUE.BOM_32BE & C'Val (0) & C'Val (0) & C'Val (0) & "A"), ASUE.UTF_32BE) = "A");
		pragma Assert (ASUE.Wide_Wide_Strings.Decode (ASUE.BOM_32 & "A") = "A");
		null;
	end;
	-- handling UTF-8 illegal sequence
	declare
		type Unsigned_31 is mod 2 ** 31;
		procedure Test (Item : Character) is
			S : String := (1 => Item);
			Last : Natural;
			Code : Wide_Wide_Character;
			Is_Illegal_Sequence : Boolean;
			D : String (1 .. 6);
		begin
			Ada.Characters.Conversions.Get (S, Last, Code, Is_Illegal_Sequence);
			pragma Assert (Is_Illegal_Sequence);
			Ada.Characters.Conversions.Put (Code, D, Last);
			pragma Assert (Unsigned_31'(Character'Pos (D (Last)) and 16#3f#) = (Character'Pos (Item) and 16#3f#));
		end Test;
	begin
		Test (Character'Val (16#80#));
		Test (Character'Val (16#8f#));
		Test (Character'Val (16#fe#));
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end utfconv;
