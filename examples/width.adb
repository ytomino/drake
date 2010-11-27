with Ada;
procedure width is
	type Ordinal_Fixed is delta 0.1 range -99.9 .. 99.9;
	type Short_Fixed is delta 0.1 digits 2;
	type Long_Fixed is delta 0.1 digits 10;
	type Enum8 is (AAA, BBB, CCC);
	type Enum16 is (AAA, BBB, CCC);
	for Enum16 use (AAA => 0, BBB => 1, CCC => 16#ffff#);
	type Enum32 is (AAA, BBB, CCC);
	for Enum32 use (AAA => 0, BBB => 1, CCC => 16#ffffffff#);
	type Unsigned is mod 2 ** 8;
	type Long_Long_Unsigned is mod 2 ** Long_Long_Integer'Size;
begin
	declare
		subtype T is Boolean range False .. Boolean'Value ("TRUE");
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Enum8 range AAA .. Enum8'Value ("CCC");
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Enum16 range AAA .. Enum16'Value ("CCC");
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Enum32 range AAA .. Enum32'Value ("CCC");
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Character range ASCII.NUL .. Character'Value ("Hex_FF");
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Wide_Character range Wide_Character'Val (0) .. Wide_Character'Value ("Hex_FFFF");
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Wide_Wide_Character range Wide_Wide_Character'Val (0) .. Wide_Wide_Character'Value ("Hex_7FFFFFFF");
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Integer range Integer'First .. Integer'Value (Integer'Image (Integer'Last));
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Unsigned range Unsigned'First .. Unsigned'Value (Unsigned'Image (Unsigned'Last));
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Float range Float'First .. Float'Value (Float'Image (Float'Last));
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Long_Float range Long_Float'First .. Long_Float'Value (Long_Float'Image (Long_Float'Last));
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Long_Long_Float range Long_Long_Float'First .. Long_Long_Float'Value (Long_Long_Float'Image (Long_Long_Float'Last));
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Ordinal_Fixed range Ordinal_Fixed'First ..
			Ordinal_Fixed'Min (
				Ordinal_Fixed'Last,
				Ordinal_Fixed'Base'Value (Ordinal_Fixed'Image (Ordinal_Fixed'Last)));
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Short_Fixed range Short_Fixed'First .. Short_Fixed'Value (Short_Fixed'Image (Short_Fixed'Last));
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
	declare
		subtype T is Long_Fixed range Long_Fixed'First .. Long_Fixed'Value (Long_Fixed'Image (Long_Fixed'Last));
	begin
		Ada.Debug.Put (Integer'Image (T'Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Width));
		Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
	end;
end width;
