with Ada.Environment_Encoding.Encoding_Streams;
with Ada.Environment_Encoding.Names;
with Ada.Environment_Encoding.Strings;
with Ada.Environment_Encoding.Wide_Strings;
with Ada.Environment_Encoding.Wide_Wide_Strings;
with Ada.Streams.Unbounded_Storage_IO;
procedure nls is
	package USIO renames Ada.Streams.Unbounded_Storage_IO;
	use type Ada.Streams.Stream_Element_Array;
	use type Ada.Streams.Stream_Element_Offset;
	Japanease_A : constant String := (
		Character'Val (16#e3#),
		Character'Val (16#81#),
		Character'Val (16#82#));
	Mongolian_Birga : constant String := (
		Character'Val (16#e1#),
		Character'Val (16#a0#),
		Character'Val (16#80#));
begin
	Ada.Debug.Put (Ada.Environment_Encoding.Image (Ada.Environment_Encoding.Current_Encoding));
	-- status check
	declare
		E : Ada.Environment_Encoding.Strings.Encoder;
		pragma Warnings (Off, E);
	begin
		if Ada.Environment_Encoding.Strings.Encode (E, "") = (1 .. 0 => <>) then
			null;
		end if;
		raise Program_Error; -- bad
	exception
		when Ada.Environment_Encoding.Status_Error =>
			null;
	end;
	-- decoding
	declare
		D : Ada.Environment_Encoding.Strings.Decoder :=
			Ada.Environment_Encoding.Strings.From (Ada.Environment_Encoding.Names.Windows_31J);
	begin
		pragma Assert (Ada.Environment_Encoding.Strings.Decode (D, (1 .. 0 => <>)) = "");
		pragma Assert (Ada.Environment_Encoding.Strings.Decode (D, (1 => 16#41#)) = "A");
		pragma Assert (Ada.Environment_Encoding.Strings.Decode (D, (16#41#, 16#42#)) = "AB");
		pragma Assert (Ada.Environment_Encoding.Strings.Decode (D, (16#82#, 16#a0#)) = Japanease_A);
		null;
	end;
	declare
		WD : Ada.Environment_Encoding.Wide_Strings.Decoder :=
			Ada.Environment_Encoding.Wide_Strings.From (Ada.Environment_Encoding.Names.Windows_31J);
	begin
		pragma Assert (Ada.Environment_Encoding.Wide_Strings.Decode (WD, (16#41#, 16#42#)) = "AB");
		null;
	end;
	declare
		WWD : Ada.Environment_Encoding.Wide_Wide_Strings.Decoder :=
			Ada.Environment_Encoding.Wide_Wide_Strings.From (Ada.Environment_Encoding.Names.Windows_31J);
	begin
		pragma Assert (Ada.Environment_Encoding.Wide_Wide_Strings.Decode (WWD, (16#41#, 16#42#)) = "AB");
		null;
	end;
	-- encoding
	declare
		E : Ada.Environment_Encoding.Strings.Encoder :=
			Ada.Environment_Encoding.Strings.To (Ada.Environment_Encoding.Names.Windows_31J);
	begin
		pragma Assert (Ada.Environment_Encoding.Strings.Encode (E, "") = (1 .. 0 => <>));
		pragma Assert (Ada.Environment_Encoding.Strings.Encode (E, "A") = (1 => 16#41#));
		pragma Assert (Ada.Environment_Encoding.Strings.Encode (E, "AB") = (16#41#, 16#42#));
		pragma Assert (Ada.Environment_Encoding.Strings.Encode (E, Japanease_A) = (16#82#, 16#a0#));
		-- substitute
		declare
			Default : constant Ada.Streams.Stream_Element_Array := Ada.Environment_Encoding.Strings.Substitute (E);
			Mongolian_Birga_In_Windows_31J : constant Ada.Streams.Stream_Element_Array :=
				Ada.Environment_Encoding.Strings.Encode (E, Mongolian_Birga);
		begin
			pragma Assert (Mongolian_Birga_In_Windows_31J = Default
				or else Mongolian_Birga_In_Windows_31J = Default & Default & Default);
			null;
		end;
		Ada.Environment_Encoding.Strings.Set_Substitute (E, (16#81#, 16#51#)); -- fullwidth low line in Windows-31J
		declare
			Mongolian_Birga_In_Windows_31J : constant Ada.Streams.Stream_Element_Array :=
				Ada.Environment_Encoding.Strings.Encode (E, Mongolian_Birga);
		begin
			pragma Assert (Mongolian_Birga_In_Windows_31J = (16#81#, 16#51#)
				or else Mongolian_Birga_In_Windows_31J = (16#81#, 16#51#, 16#81#, 16#51#, 16#81#, 16#51#));
			null;
		end;
	end;
	declare
		WE : Ada.Environment_Encoding.Wide_Strings.Encoder :=
			Ada.Environment_Encoding.Wide_Strings.To (Ada.Environment_Encoding.Names.Windows_31J);
	begin
		pragma Assert (Ada.Environment_Encoding.Wide_Strings.Encode (WE, "AB") = (16#41#, 16#42#));
		null;
	end;
	declare
		WWE : Ada.Environment_Encoding.Wide_Wide_Strings.Encoder :=
			Ada.Environment_Encoding.Wide_Wide_Strings.To (Ada.Environment_Encoding.Names.Windows_31J);
	begin
		pragma Assert (Ada.Environment_Encoding.Wide_Wide_Strings.Encode (WWE, "AB") = (16#41#, 16#42#));
		null;
	end;
	-- reading
	declare
		Buffer : USIO.Buffer_Type;
		E : aliased Ada.Environment_Encoding.Encoding_Streams.Inout_Type :=
			Ada.Environment_Encoding.Encoding_Streams.Open (
				Ada.Environment_Encoding.Names.UTF_8,
				Ada.Environment_Encoding.Names.Windows_31J,
				USIO.Stream (Buffer));
		S : String (1 .. 3);
		One_Element : String (1 .. 1);
	begin
		for I in 1 .. 100 loop
			Ada.Streams.Write (
				USIO.Stream (Buffer).all,
				(16#82#, 16#a0#));
		end loop;
		Ada.Streams.Set_Index (
			Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all),
			1);
		for I in 1 .. 100 loop
			String'Read (
				Ada.Environment_Encoding.Encoding_Streams.Stream (E),
				S);
			pragma Assert (S = Japanease_A);
		end loop;
		begin
			String'Read (
				Ada.Environment_Encoding.Encoding_Streams.Stream (E),
				One_Element);
			raise Program_Error;
		exception
			when Ada.Environment_Encoding.Encoding_Streams.End_Error =>
				null;
		end;
	end;
	-- writing
	declare
		Buffer : USIO.Buffer_Type;
		E : aliased Ada.Environment_Encoding.Encoding_Streams.Inout_Type :=
			Ada.Environment_Encoding.Encoding_Streams.Open (
				Ada.Environment_Encoding.Names.Windows_31J,
				Ada.Environment_Encoding.Names.UTF_8,
				USIO.Stream (Buffer));
		S : String (1 .. 3);
	begin
		for I in 1 .. 100 loop
			Ada.Streams.Write (
				Ada.Environment_Encoding.Encoding_Streams.Stream (E).all,
				(16#82#, 16#a0#));
		end loop;
		Ada.Environment_Encoding.Encoding_Streams.Finish (E);
		Ada.Streams.Set_Index (
			Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all),
			1);
		pragma Assert (Ada.Streams.Stream_Element_Count'(USIO.Size (Buffer)) = 300);
		for I in 1 .. 100 loop
			String'Read (
				USIO.Stream (Buffer),
				S);
			pragma Assert (S = Japanease_A);
		end loop;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end nls;
