with Ada.Exceptions;
with Ada.Float;
procedure image is
	type Ordinal_Fixed is delta 0.1 range -99.9 .. 99.9;
	type Short_Fixed is delta 0.1 digits 2;
	type Long_Fixed is delta 0.1 digits 10;
	type Enum8 is (AAA, BBB, CCC);
	type Enum16 is (AAA, BBB, CCC);
	for Enum16 use (AAA => 0, BBB => 1, CCC => 16#ffff#);
	type Enum32 is (AAA, BBB, CCC);
	for Enum32 use (AAA => 0, BBB => 1, CCC => 16#ffffffff#);
	type Short_Short_Unsigned is mod 2 ** 8;
	type Long_Long_Unsigned is mod 2 ** Long_Long_Integer'Size;
begin
	-- 'Image
	declare
		function Image (X : Boolean) return String renames Boolean'Image;
		function Image (X : Enum8) return String renames Enum8'Image;
		function Image (X : Enum16) return String renames Enum16'Image;
		function Image (X : Enum32) return String renames Enum32'Image;
		V : Integer;
	begin
		pragma Assert (Boolean'Image (Boolean'First) = "FALSE");
		pragma Assert (Image (Boolean'Last) = "TRUE");
		begin
			V := Boolean'Pos (Boolean'Value ("XYZ"));
		exception
			when E : Constraint_Error =>
				pragma Assert (Ada.Exceptions.Exception_Message (E) = "Boolean'Value (""XYZ"")");
				null;
		end;
		pragma Assert (Enum8'Image (Enum8'First) = "AAA");
		pragma Assert (Image (Enum8'Last) = "CCC");
		pragma Assert (Enum16'Image (Enum16'First) = "AAA");
		pragma Assert (Image (Enum16'Last) = "CCC");
		pragma Assert (Enum32'Image (Enum32'First) = "AAA");
		pragma Assert (Image (Enum32'Last) = "CCC");
		pragma Assert (Character'Image (Character'First) = "NUL");
		pragma Assert (Character'Image (Character'Val (16#ad#)) = "Hex_AD");
		pragma Assert (Character'Image (Character'Last) = "Hex_FF");
		pragma Assert (Wide_Character'Image (Wide_Character'First) = "NUL");
		pragma Assert (Wide_Character'Image (Wide_Character'Val (16#ad#)) = "SOFT_HYPHEN");
		pragma Assert (Wide_Character'Image (Wide_Character'Last) = "Hex_FFFF");
		pragma Assert (Wide_Wide_Character'Image (Wide_Wide_Character'First) = "NUL");
		pragma Assert (Wide_Wide_Character'Image (Wide_Wide_Character'Val (16#ad#)) = "SOFT_HYPHEN");
		pragma Assert (Wide_Wide_Character'Image (Wide_Wide_Character'Last) = "Hex_7FFFFFFF");
		Ada.Debug.Put (Integer'Image (Integer'First));
		Ada.Debug.Put (Integer'Image (Integer'Last));
		Ada.Debug.Put (Long_Long_Integer'Image (Long_Long_Integer'First));
		Ada.Debug.Put (Long_Long_Integer'Image (Long_Long_Integer'Last));
		pragma Assert (Short_Short_Unsigned'Image (Short_Short_Unsigned'First) = " 0");
		pragma Assert (Short_Short_Unsigned'Image (Short_Short_Unsigned'Last) = " 255");
		pragma Assert (Long_Long_Unsigned'Image (Long_Long_Unsigned'First) = " 0");
		Ada.Debug.Put (Long_Long_Unsigned'Image (Long_Long_Unsigned'Last));
		Ada.Debug.Put (Float'Image (Float'First));
		Ada.Debug.Put (Float'Image (Float'Last));
		Ada.Debug.Put (Long_Float'Image (Long_Float'First));
		Ada.Debug.Put (Long_Float'Image (Long_Float'Last));
		Ada.Debug.Put (Long_Long_Float'Image (Long_Long_Float'First));
		Ada.Debug.Put (Long_Long_Float'Image (Long_Long_Float'Last));
		pragma Assert (Ordinal_Fixed'Image (Ordinal_Fixed'First) = "-99.9");
		pragma Assert (Ordinal_Fixed'Image (Ordinal_Fixed'Last) = " 99.9");
		pragma Assert (Short_Fixed'Image (Short_Fixed'First) = "-9.9");
		pragma Assert (Short_Fixed'Image (Short_Fixed'Last) = " 9.9");
		pragma Assert (Long_Fixed'Image (Long_Fixed'First) = "-999999999.9");
		pragma Assert (Long_Fixed'Image (Long_Fixed'Last) = " 999999999.9");
	end;
	-- 'Value
	declare
		function "=" (Left, Right : Float) return Boolean is
			function Is_Infinity is new Ada.Float.Is_Infinity (Float);
		begin
			pragma Assert (not Is_Infinity (Left));
			pragma Assert (not Is_Infinity (Right));
			return abs (Left - Right) / Float'Max (abs Left, abs Right) < Float'Model_Epsilon * 100.0;
		end "=";
		function "=" (Left, Right : Long_Float) return Boolean is
			function Is_Infinity is new Ada.Float.Is_Infinity (Long_Float);
		begin
			pragma Assert (not Is_Infinity (Left));
			pragma Assert (not Is_Infinity (Right));
			return abs (Left - Right) / Long_Float'Max (abs Left, abs Right) < Long_Float'Model_Epsilon * 100.0;
		end "=";
		function "=" (Left, Right : Long_Long_Float) return Boolean is
			function Is_Infinity is new Ada.Float.Is_Infinity (Long_Long_Float);
		begin
			pragma Assert (not Is_Infinity (Left));
			pragma Assert (not Is_Infinity (Right));
			return abs (Left - Right) / Long_Long_Float'Max (abs Left, abs Right) < Long_Long_Float'Model_Epsilon * 100.0;
		end "=";
		function "=" (Left, Right : Ordinal_Fixed) return Boolean is
		begin
			return Ordinal_Fixed'Image (Left) = Ordinal_Fixed'Image (Right);
		end "=";
	begin
		pragma Assert (Boolean'Value (Boolean'Image (Boolean'First)) = Boolean'First);
		pragma Assert (Boolean'Value (Boolean'Image (Boolean'Last)) = Boolean'Last);
		pragma Assert (Enum8'Value (Enum8'Image (Enum8'First)) = Enum8'First);
		pragma Assert (Enum8'Value (Enum8'Image (Enum8'Last)) = Enum8'Last);
		pragma Assert (Enum16'Value (Enum16'Image (Enum16'First)) = Enum16'First);
		pragma Assert (Enum16'Value (Enum16'Image (Enum16'Last)) = Enum16'Last);
		pragma Assert (Enum32'Value (Enum32'Image (Enum32'First)) = Enum32'First);
		pragma Assert (Enum32'Value (Enum32'Image (Enum32'Last)) = Enum32'Last);
		begin
			if Character'Value ("SOFT_HYPHEN") = Character'Val (16#ad#) then
				null;
			end if;
			pragma Assert (False);
		exception
			when Constraint_Error => null; -- OK
		end;
		for I in Character loop
			pragma Assert (Character'Value (Character'Image (I)) = I);
			null;
		end loop;
		pragma Assert (Wide_Character'Value (Wide_Character'Image (Wide_Character'First)) = Wide_Character'First);
		pragma Assert (Wide_Character'Value ("SOFT_HYPHEN") = Wide_Character'Val (16#ad#));
		pragma Assert (Wide_Character'Value (Wide_Character'Image (Wide_Character'Last)) = Wide_Character'Last);
		pragma Assert (Wide_Wide_Character'Value ("Hex_00000000") = Wide_Wide_Character'First);
		pragma Assert (Wide_Wide_Character'Value ("SOFT_HYPHEN") = Wide_Wide_Character'Val (16#ad#));
		pragma Assert (Wide_Wide_Character'Value ("Hex_7fffffff") = Wide_Wide_Character'Last);
		pragma Assert (Integer'Value (Integer'Image (Integer'First)) = Integer'First);
		pragma Assert (Integer'Value (Integer'Image (Integer'Last)) = Integer'Last);
		pragma Assert (Long_Long_Integer'Value (Long_Long_Integer'Image (Long_Long_Integer'First)) = Long_Long_Integer'First);
		pragma Assert (Long_Long_Integer'Value (Long_Long_Integer'Image (Long_Long_Integer'Last)) = Long_Long_Integer'Last);
		pragma Assert (Short_Short_Unsigned'Value (Short_Short_Unsigned'Image (Short_Short_Unsigned'First)) = Short_Short_Unsigned'First);
		pragma Assert (Short_Short_Unsigned'Value (Short_Short_Unsigned'Image (Short_Short_Unsigned'Last)) = Short_Short_Unsigned'Last);
		pragma Assert (Long_Long_Unsigned'Value (Long_Long_Unsigned'Image (Long_Long_Unsigned'First)) = Long_Long_Unsigned'First);
		pragma Assert (Long_Long_Unsigned'Value (Long_Long_Unsigned'Image (Long_Long_Unsigned'Last)) = Long_Long_Unsigned'Last);
		pragma Assert (Float'Value (Float'Image (Float'First)) = Float'First);
		pragma Assert (Float'Value (Float'Image (Float'Last)) = Float'Last);
		pragma Assert (Long_Float'Value (Long_Float'Image (Long_Float'First * 0.999999999999999)) = Long_Float'First);
		pragma Assert (Long_Float'Value (Long_Float'Image (Long_Float'Last * 0.999999999999999)) = Long_Float'Last);
		pragma Assert (Long_Long_Float'Value (Long_Long_Float'Image (Long_Long_Float'First * 0.999999999999999999)) = Long_Long_Float'First);
		pragma Assert (Long_Long_Float'Value (Long_Long_Float'Image (Long_Long_Float'Last * 0.999999999999999999)) = Long_Long_Float'Last);
		pragma Assert (Long_Long_Float'Value ("0.01") = 0.01);
		pragma Assert (Long_Long_Float'Value ("0.001") = 0.001);
		pragma Assert (Ordinal_Fixed'Value (Ordinal_Fixed'Image (Ordinal_Fixed'First)) = Ordinal_Fixed'First);
		pragma Assert (Ordinal_Fixed'Value (Ordinal_Fixed'Image (Ordinal_Fixed'Last)) = Ordinal_Fixed'Last);
		pragma Assert (Short_Fixed'Value (Short_Fixed'Image (Short_Fixed'First)) = Short_Fixed'First);
		pragma Assert (Short_Fixed'Value (Short_Fixed'Image (Short_Fixed'Last)) = Short_Fixed'Last);
		pragma Assert (Long_Fixed'Value (Long_Fixed'Image (Long_Fixed'First)) = Long_Fixed'First);
		pragma Assert (Long_Fixed'Value (Long_Fixed'Image (Long_Fixed'Last)) = Long_Fixed'Last);
	end;
	-- 'Width
	begin
		declare
			subtype T is Boolean range False .. Boolean'Value ("TRUE");
		begin
			pragma Assert (T'Width = 5);
			pragma Assert (T'Wide_Width = 5);
			pragma Assert (T'Wide_Wide_Width = 5);
			null;
		end;
		declare
			subtype T is Enum8 range AAA .. Enum8'Value ("CCC");
		begin
			pragma Assert (T'Width = 3);
			pragma Assert (T'Wide_Width = 3);
			pragma Assert (T'Wide_Wide_Width = 3);
			null;
		end;
		declare
			subtype T is Enum16 range AAA .. Enum16'Value ("CCC");
		begin
			pragma Assert (T'Width = 3);
			pragma Assert (T'Wide_Width = 3);
			pragma Assert (T'Wide_Wide_Width = 3);
			null;
		end;
		declare
			subtype T is Enum32 range AAA .. Enum32'Value ("CCC");
		begin
			pragma Assert (T'Width = 3);
			pragma Assert (T'Wide_Width = 3);
			pragma Assert (T'Wide_Wide_Width = 3);
			null;
		end;
		declare
			subtype T is Character range ASCII.NUL .. Character'Value ("Hex_FF");
		begin
			pragma Assert (T'Width = 6); -- "Hex_XX"
			pragma Assert (T'Wide_Width = 6);
			pragma Assert (T'Wide_Wide_Width = 6);
			null;
		end;
		for I in Character'First .. Character'Val (16#80#) loop
			for J in I .. Character'Val (16#80#) loop
				declare
					subtype T is Character range I .. J;
					Max_Width : Natural := 0;
				begin
					for K in T loop
						Max_Width := Natural'Max (Max_Width, T'Image (K)'Length);
					end loop;
					pragma Assert (T'Width = Max_Width);
					pragma Assert (T'Wide_Width = Max_Width);
					pragma Assert (T'Wide_Wide_Width = Max_Width);
					null;
				end;
			end loop;
		end loop;
		declare
			subtype T is Wide_Character range Wide_Character'Val (0) .. Wide_Character'Value ("Hex_FFFF");
		begin
			pragma Assert (T'Width = 8); -- "Hex_XXXX"
			pragma Assert (T'Wide_Width = 8);
			pragma Assert (T'Wide_Wide_Width = 8);
			null;
		end;
		declare
			subtype T is Wide_Wide_Character range Wide_Wide_Character'Val (0) .. Wide_Wide_Character'Value ("Hex_7FFFFFFF");
		begin
			pragma Assert (T'Width = 12); -- "Hex_XXXXXXXX"
			pragma Assert (T'Wide_Width = 12);
			pragma Assert (T'Wide_Wide_Width = 12);
			null;
		end;
		declare
			subtype T is Integer range Integer'First .. Integer'Value (Integer'Image (Integer'Last));
			Widest_Image : constant String := Integer'Image (Integer'First);
		begin
			pragma Assert (T'Width = Widest_Image'Length);
			pragma Assert (T'Wide_Width = Widest_Image'Length);
			pragma Assert (T'Wide_Wide_Width = Widest_Image'Length);
			null;
		end;
		declare
			subtype T is Short_Short_Unsigned range
				Short_Short_Unsigned'First ..
				Short_Short_Unsigned'Value (Short_Short_Unsigned'Image (Short_Short_Unsigned'Last));
			Widest_Image : constant String := Short_Short_Unsigned'Image (Short_Short_Unsigned'Last);
		begin
			pragma Assert (T'Width = Widest_Image'Length);
			pragma Assert (T'Wide_Width = Widest_Image'Length);
			pragma Assert (T'Wide_Wide_Width = Widest_Image'Length);
			null;
		end;
		declare
			subtype T is Float range Float'First .. Float'Value (Float'Image (Float'Last));
			Widest_Image : constant String := Float'Image (Float'First);
		begin
			pragma Assert (T'Width = Widest_Image'Length);
			pragma Assert (T'Wide_Width = Widest_Image'Length);
			pragma Assert (T'Wide_Wide_Width = Widest_Image'Length);
			null;
		end;
		declare
			subtype T is Long_Float range
				Long_Float'First ..
				Long_Float'Min (
					Long_Float'Last,
					Long_Float'Value (Long_Float'Image (Long_Float'Last)));
			Widest_Image : constant String := Long_Float'Image (Long_Float'First);
		begin
			pragma Assert (T'Width = Widest_Image'Length);
			pragma Assert (T'Wide_Width = Widest_Image'Length);
			pragma Assert (T'Wide_Wide_Width = Widest_Image'Length);
			null;
		end;
		declare
			subtype T is Long_Long_Float range
				Long_Long_Float'First ..
				Long_Long_Float'Min (
					Long_Long_Float'Last,
					Long_Long_Float'Value (Long_Long_Float'Image (Long_Long_Float'Last)));
			Widest_Image : constant String := Long_Long_Float'Image (Long_Long_Float'First);
		begin
			pragma Assert (T'Width = Widest_Image'Length);
			pragma Assert (T'Wide_Width = Widest_Image'Length);
			pragma Assert (T'Wide_Wide_Width = Widest_Image'Length);
			null;
		end;
		declare
			subtype T is Ordinal_Fixed range
				Ordinal_Fixed'First ..
				Ordinal_Fixed'Min (
					Ordinal_Fixed'Last,
					Ordinal_Fixed'Base'Value (Ordinal_Fixed'Image (Ordinal_Fixed'Last)));
		begin
			Ada.Debug.Put (Integer'Image (T'Width));
			Ada.Debug.Put (Integer'Image (T'Wide_Width));
			Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
			pragma Assert (T'Fore = 3); -- "-99"
			null;
		end;
		declare
			subtype T is Short_Fixed range Short_Fixed'First .. Short_Fixed'Value (Short_Fixed'Image (Short_Fixed'Last));
		begin
			Ada.Debug.Put (Integer'Image (T'Width));
			Ada.Debug.Put (Integer'Image (T'Wide_Width));
			Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
			pragma Assert (T'Fore = 2); -- "-0"
			null;
		end;
		declare
			subtype T is Long_Fixed range Long_Fixed'First .. Long_Fixed'Value (Long_Fixed'Image (Long_Fixed'Last));
		begin
			Ada.Debug.Put (Integer'Image (T'Width));
			Ada.Debug.Put (Integer'Image (T'Wide_Width));
			Ada.Debug.Put (Integer'Image (T'Wide_Wide_Width));
			pragma Assert (T'Fore = 10); -- sign + 9 columns
			null;
		end;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end image;
