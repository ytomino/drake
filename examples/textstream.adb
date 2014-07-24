with Ada.Environment_Variables;
with Ada.IO_Modes;
with Ada.Streams.Unbounded_Storage_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Wide_Text_IO.Text_Streams;
with Ada.Wide_Wide_Text_IO.Text_Streams;
procedure textstream is
	use type Ada.Streams.Stream_Element_Offset;
	use type Ada.Text_IO.Count;
	package U renames Ada.Streams.Unbounded_Storage_IO;
	Windows : constant Boolean :=
		Ada.Environment_Variables.Exists ("OS")
		and then Ada.Environment_Variables.Value ("OS") = "Windows_NT";
	Aegean_Number_One_In_UTF_8 : constant String (1 .. 4) := (
		Character'Val (16#F0#),
		Character'Val (16#90#),
		Character'Val (16#84#),
		Character'Val (16#87#));
	Aegean_Number_One_In_UTF_16 : constant Wide_String (1 .. 2) := (
		Wide_Character'Val (16#d800#),
		Wide_Character'Val (16#dd07#));
	Aegean_Number_One_In_UTF_32 : constant Wide_Wide_String (1 .. 1) :=
		(1 => Wide_Wide_Character'Val (16#00010107#));
	Japanease_A_In_UTF_8 : constant String := (
		Character'Val (16#e3#),
		Character'Val (16#81#),
		Character'Val (16#82#));
	Japanease_A_In_UTF_16 : constant Wide_String :=
		(1 => Wide_Character'Val (16#3042#));
	Japanease_A_In_UTF_32 : constant Wide_Wide_String :=
		(1 => Wide_Wide_Character'Val (16#3042#));
	Japanease_A_In_SJIS : constant String := (
		Character'Val (16#82#),
		Character'Val (16#a0#));
begin
	-- writing a legal sequence
	declare
		procedure Process (
			External : Ada.IO_Modes.File_External_Spec;
			Stream_Data : String;
			String_Data : String;
			Wide_String_Data : Wide_String;
			Wide_Wide_String_Data : Wide_Wide_String)
		is
			Buffer : U.Buffer_Type;
			Data : String (Stream_Data'Range);
		begin
			declare
				File : Ada.Text_IO.File_Type;
			begin
				Ada.Text_IO.Text_Streams.Open (
					File,
					Ada.Text_IO.Out_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in String_Data'Range loop
					Ada.Text_IO.Put (File, String_Data (I));
				end loop;
				pragma Assert (Ada.Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Text_IO.Close (File);
				pragma Assert (U.Size (Buffer) = Stream_Data'Length);
				U.Reset (Buffer);
				String'Read (U.Stream (Buffer), Data);
				pragma Assert (Data = Stream_Data);
			end;
			U.Set_Size (Buffer, 0);
			declare
				File : Ada.Wide_Text_IO.File_Type;
			begin
				Ada.Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Text_IO.Out_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_String_Data'Range loop
					Ada.Wide_Text_IO.Put (File, Wide_String_Data (I));
				end loop;
				pragma Assert (Ada.Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Text_IO.Close (File);
				pragma Assert (U.Size (Buffer) = Stream_Data'Length);
				U.Reset (Buffer);
				String'Read (U.Stream (Buffer), Data);
				pragma Assert (Data = Stream_Data);
			end;
			U.Set_Size (Buffer, 0);
			declare
				File : Ada.Wide_Wide_Text_IO.File_Type;
			begin
				Ada.Wide_Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Wide_Text_IO.Out_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_Wide_String_Data'Range loop
					Ada.Wide_Wide_Text_IO.Put (File, Wide_Wide_String_Data (I));
				end loop;
				pragma Assert (Ada.Wide_Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Wide_Text_IO.Close (File);
				pragma Assert (U.Size (Buffer) = Stream_Data'Length);
				U.Reset (Buffer);
				String'Read (U.Stream (Buffer), Data);
				pragma Assert (Data = Stream_Data);
			end;
		end Process;
	begin
		-- UTF-8
		Process (
			Ada.IO_Modes.UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_16,
			Aegean_Number_One_In_UTF_32);
		-- DBCS (SJIS)
		if Windows then
			Process (
				Ada.IO_Modes.Locale,
				Japanease_A_In_SJIS,
				Japanease_A_In_UTF_8,
				Japanease_A_In_UTF_16,
				Japanease_A_In_UTF_32);
		end if;
	end;
	-- reading a legal sequence
	declare
		procedure Process (
			External : Ada.IO_Modes.File_External_Spec;
			Stream_Data : String;
			String_Data : String;
			Wide_String_Data : Wide_String;
			Wide_Wide_String_Data : Wide_Wide_String)
		is
			Buffer : U.Buffer_Type;
		begin
			String'Write (U.Stream (Buffer), Stream_Data);
			U.Reset (Buffer);
			declare
				File : Ada.Text_IO.File_Type;
				Item : Character;
			begin
				Ada.Text_IO.Text_Streams.Open (
					File,
					Ada.Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in String_Data'Range loop
					Ada.Text_IO.Get (File, Item);
					pragma Assert (Item = String_Data (I));
				end loop;
				pragma Assert (Ada.Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Text_IO.Close (File);
			end;
			U.Reset (Buffer);
			declare
				File : Ada.Wide_Text_IO.File_Type;
				Item : Wide_Character;
			begin
				Ada.Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_String_Data'Range loop
					Ada.Wide_Text_IO.Get (File, Item);
					pragma Assert (Item = Wide_String_Data (I));
				end loop;
				pragma Assert (Ada.Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Text_IO.Close (File);
			end;
			U.Reset (Buffer);
			declare
				File : Ada.Wide_Wide_Text_IO.File_Type;
				Item : Wide_Wide_Character;
			begin
				Ada.Wide_Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Wide_Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_Wide_String_Data'Range loop
					Ada.Wide_Wide_Text_IO.Get (File, Item);
					pragma Assert (Item = Wide_Wide_String_Data (I));
				end loop;
				pragma Assert (Ada.Wide_Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Wide_Text_IO.Close (File);
			end;
		end Process;
	begin
		-- UTF-8
		Process (
			Ada.IO_Modes.UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_16,
			Aegean_Number_One_In_UTF_32);
		-- DBCS (SJIS)
		if Windows then
			Process (
				Ada.IO_Modes.Locale,
				Japanease_A_In_SJIS,
				Japanease_A_In_UTF_8,
				Japanease_A_In_UTF_16,
				Japanease_A_In_UTF_32);
		end if;
	end;
	-- reading a legal sequence with calling End_Of_*
	declare
		procedure Process (
			External : Ada.IO_Modes.File_External_Spec;
			Stream_Data : String;
			String_Data : String;
			Wide_String_Data : Wide_String;
			Wide_Wide_String_Data : Wide_Wide_String)
		is
			Buffer : U.Buffer_Type;
		begin
			String'Write (U.Stream (Buffer), Stream_Data);
			U.Reset (Buffer);
			declare
				File : Ada.Text_IO.File_Type;
				Item : Character;
			begin
				Ada.Text_IO.Text_Streams.Open (
					File,
					Ada.Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in String_Data'Range loop
					pragma Assert (not Ada.Text_IO.End_Of_Line (File));
					pragma Assert (not Ada.Text_IO.End_Of_Page (File));
					pragma Assert (not Ada.Text_IO.End_Of_File (File));
					Ada.Text_IO.Get (File, Item);
					pragma Assert (Item = String_Data (I));
				end loop;
				pragma Assert (Ada.Text_IO.End_Of_Line (File));
				pragma Assert (Ada.Text_IO.End_Of_Page (File));
				pragma Assert (Ada.Text_IO.End_Of_File (File));
				pragma Assert (Ada.Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Text_IO.Close (File);
			end;
			U.Reset (Buffer);
			declare
				File : Ada.Wide_Text_IO.File_Type;
				Item : Wide_Character;
			begin
				Ada.Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_String_Data'Range loop
					pragma Assert (not Ada.Wide_Text_IO.End_Of_Line (File));
					pragma Assert (not Ada.Wide_Text_IO.End_Of_Page (File));
					pragma Assert (not Ada.Wide_Text_IO.End_Of_File (File));
					Ada.Wide_Text_IO.Get (File, Item);
					pragma Assert (Item = Wide_String_Data (I));
				end loop;
				pragma Assert (Ada.Wide_Text_IO.End_Of_Line (File));
				pragma Assert (Ada.Wide_Text_IO.End_Of_Page (File));
				pragma Assert (Ada.Wide_Text_IO.End_Of_File (File));
				pragma Assert (Ada.Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Text_IO.Close (File);
			end;
			U.Reset (Buffer);
			declare
				File : Ada.Wide_Wide_Text_IO.File_Type;
				Item : Wide_Wide_Character;
			begin
				Ada.Wide_Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Wide_Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_Wide_String_Data'Range loop
					pragma Assert (not Ada.Wide_Wide_Text_IO.End_Of_Line (File));
					pragma Assert (not Ada.Wide_Wide_Text_IO.End_Of_Page (File));
					pragma Assert (not Ada.Wide_Wide_Text_IO.End_Of_File (File));
					Ada.Wide_Wide_Text_IO.Get (File, Item);
					pragma Assert (Item = Wide_Wide_String_Data (I));
				end loop;
				pragma Assert (Ada.Wide_Wide_Text_IO.End_Of_Line (File));
				pragma Assert (Ada.Wide_Wide_Text_IO.End_Of_Page (File));
				pragma Assert (Ada.Wide_Wide_Text_IO.End_Of_File (File));
				pragma Assert (Ada.Wide_Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Wide_Text_IO.Close (File);
			end;
		end Process;
	begin
		-- UTF-8
		Process (
			Ada.IO_Modes.UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_16,
			Aegean_Number_One_In_UTF_32);
		-- DBCS (SJIS)
		if Windows then
			Process (
				Ada.IO_Modes.Locale,
				Japanease_A_In_SJIS,
				Japanease_A_In_UTF_8,
				Japanease_A_In_UTF_16,
				Japanease_A_In_UTF_32);
		end if;
	end;
	-- reading a legal sequence by Get_Immediate
	declare
		procedure Process (
			External : Ada.IO_Modes.File_External_Spec;
			Stream_Data : String;
			String_Data : String;
			Wide_String_Data : Wide_String;
			Wide_Wide_String_Data : Wide_Wide_String)
		is
			Buffer : U.Buffer_Type;
		begin
			String'Write (U.Stream (Buffer), Stream_Data);
			U.Reset (Buffer);
			declare
				File : Ada.Text_IO.File_Type;
				Item : Character;
			begin
				Ada.Text_IO.Text_Streams.Open (
					File,
					Ada.Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in String_Data'Range loop
					Ada.Text_IO.Get_Immediate (File, Item);
					pragma Assert (Item = String_Data (I));
				end loop;
				pragma Assert (Ada.Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Text_IO.Close (File);
			end;
			U.Reset (Buffer);
			declare
				File : Ada.Wide_Text_IO.File_Type;
				Item : Wide_Character;
			begin
				Ada.Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_String_Data'Range loop
					Ada.Wide_Text_IO.Get_Immediate (File, Item);
					pragma Assert (Item = Wide_String_Data (I));
				end loop;
				pragma Assert (Ada.Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Text_IO.Close (File);
			end;
			U.Reset (Buffer);
			declare
				File : Ada.Wide_Wide_Text_IO.File_Type;
				Item : Wide_Wide_Character;
			begin
				Ada.Wide_Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Wide_Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_Wide_String_Data'Range loop
					Ada.Wide_Wide_Text_IO.Get_Immediate (File, Item);
					pragma Assert (Item = Wide_Wide_String_Data (I));
				end loop;
				pragma Assert (Ada.Wide_Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Wide_Text_IO.Close (File);
			end;
		end Process;
	begin
		-- UTF-8
		Process (
			Ada.IO_Modes.UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_16,
			Aegean_Number_One_In_UTF_32);
		-- DBCS (SJIS)
		if Windows then
			Process (
				Ada.IO_Modes.Locale,
				Japanease_A_In_SJIS,
				Japanease_A_In_UTF_8,
				Japanease_A_In_UTF_16,
				Japanease_A_In_UTF_32);
		end if;
	end;
	-- reading a legal sequence with calling Look_Ahead
	declare
		procedure Process (
			External : Ada.IO_Modes.File_External_Spec;
			Stream_Data : String;
			String_Data : String;
			Wide_String_Data : Wide_String;
			Wide_Wide_String_Data : Wide_Wide_String)
		is
			Buffer : U.Buffer_Type;
		begin
			String'Write (U.Stream (Buffer), Stream_Data);
			U.Reset (Buffer);
			declare
				File : Ada.Text_IO.File_Type;
				Item : Character;
				End_Of_Line : Boolean;
			begin
				Ada.Text_IO.Text_Streams.Open (
					File,
					Ada.Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in String_Data'Range loop
					Ada.Text_IO.Look_Ahead (File, Item, End_Of_Line);
					pragma Assert (not End_Of_Line);
					pragma Assert (Item = String_Data (I));
					Ada.Text_IO.Get (File, Item);
					pragma Assert (Item = String_Data (I));
				end loop;
				Ada.Text_IO.Look_Ahead (File, Item, End_Of_Line);
				pragma Assert (End_Of_Line);
				pragma Assert (Ada.Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Text_IO.Close (File);
			end;
			U.Reset (Buffer);
			declare
				File : Ada.Wide_Text_IO.File_Type;
				Item : Wide_Character;
				End_Of_Line : Boolean;
			begin
				Ada.Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_String_Data'Range loop
					Ada.Wide_Text_IO.Look_Ahead (File, Item, End_Of_Line);
					pragma Assert (not End_Of_Line);
					pragma Assert (Item = Wide_String_Data (I));
					Ada.Wide_Text_IO.Get (File, Item);
					pragma Assert (Item = Wide_String_Data (I));
				end loop;
				Ada.Wide_Text_IO.Look_Ahead (File, Item, End_Of_Line);
				pragma Assert (End_Of_Line);
				pragma Assert (Ada.Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Text_IO.Close (File);
			end;
			U.Reset (Buffer);
			declare
				File : Ada.Wide_Wide_Text_IO.File_Type;
				Item : Wide_Wide_Character;
				End_Of_Line : Boolean;
			begin
				Ada.Wide_Wide_Text_IO.Text_Streams.Open (
					File,
					Ada.Wide_Wide_Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in Wide_Wide_String_Data'Range loop
					Ada.Wide_Wide_Text_IO.Look_Ahead (File, Item, End_Of_Line);
					pragma Assert (not End_Of_Line);
					pragma Assert (Item = Wide_Wide_String_Data (I));
					Ada.Wide_Wide_Text_IO.Get_Immediate (File, Item);
					pragma Assert (Item = Wide_Wide_String_Data (I));
				end loop;
				Ada.Wide_Wide_Text_IO.Look_Ahead (File, Item, End_Of_Line);
				pragma Assert (End_Of_Line);
				pragma Assert (Ada.Wide_Wide_Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Wide_Wide_Text_IO.Close (File);
			end;
		end Process;
	begin
		-- UTF-8
		Process (
			Ada.IO_Modes.UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_8,
			Aegean_Number_One_In_UTF_16,
			Aegean_Number_One_In_UTF_32);
		-- DBCS (SJIS)
		if Windows then
			Process (
				Ada.IO_Modes.Locale,
				Japanease_A_In_SJIS,
				Japanease_A_In_UTF_8,
				Japanease_A_In_UTF_16,
				Japanease_A_In_UTF_32);
		end if;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end textstream;
