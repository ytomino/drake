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
	Illegal_UTF_8 : constant String (1 .. 10) := (
		Character'Val (16#F0#),
		Character'Val (16#F0#),
		Character'Val (16#90#),
		Character'Val (16#F0#),
		Character'Val (16#90#),
		Character'Val (16#84#),
		Character'Val (16#F0#),
		Character'Val (16#90#),
		Character'Val (16#84#),
		Character'Val (16#87#));
	Illegal_UTF_8_As_UTF_16 : constant Wide_String (1 .. 7) := (
		Wide_Character'Val (16#0000#),
		Wide_Character'Val (16#d800#),
		Wide_Character'Val (16#dc00#),
		Wide_Character'Val (16#d800#),
		Wide_Character'Val (16#dd00#),
		Wide_Character'Val (16#d800#),
		Wide_Character'Val (16#dd07#));
	Illegal_UTF_8_As_UTF_32 : constant Wide_Wide_String (1 .. 4) := (
		Wide_Wide_Character'Val (16#00000000#),
		Wide_Wide_Character'Val (16#00010000#),
		Wide_Wide_Character'Val (16#00010100#),
		Wide_Wide_Character'Val (16#00010107#));
	Illegal_UTF_16 : constant Wide_String (1 .. 3) := (
		Wide_Character'Val (16#d800#),
		Wide_Character'Val (16#d800#),
		Wide_Character'Val (16#dd07#));
	Illegal_UTF_16_As_UTF_8 : constant String (1 .. 7) := (
		Character'Val (16#ED#),
		Character'Val (16#A0#),
		Character'Val (16#80#),
		Character'Val (16#F0#),
		Character'Val (16#90#),
		Character'Val (16#84#),
		Character'Val (16#87#));
	Japanease_A_In_UTF_8 : constant String (1 .. 6) := (
		Character'Val (16#EF#), -- halfwidth katakana
		Character'Val (16#BD#),
		Character'Val (16#B1#),
		Character'Val (16#e3#), -- hiragana
		Character'Val (16#81#),
		Character'Val (16#82#));
	Japanease_A_In_UTF_16 : constant Wide_String (1 .. 2) := (
		Wide_Character'Val (16#FF71#), -- halfwidth katakana
		Wide_Character'Val (16#3042#)); -- hiragana
	Japanease_A_In_UTF_32 : constant Wide_Wide_String (1 .. 2) := (
		Wide_Wide_Character'Val (16#FF71#), -- halfwidth katakana
		Wide_Wide_Character'Val (16#3042#)); -- hiragana
	Japanease_A_In_SJIS : constant String (1 .. 3) := (
		Character'Val (16#B1#), -- halfwidth katakana
		Character'Val (16#82#), -- hiragana
		Character'Val (16#a0#));
begin
	-- writing
	declare
		generic
			type Character_Type is (<>);
			type String_Type is array (Positive range <>) of Character_Type;
			with procedure Put (File : in Ada.Text_IO.File_Type; Item : in Character_Type);
		procedure Generic_Process (External : in Ada.IO_Modes.File_External_Spec; Stream_Data : in String; String_Data : in String_Type);
		procedure Generic_Process (External : in Ada.IO_Modes.File_External_Spec; Stream_Data : in String; String_Data : in String_Type) is
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
					Put (File, String_Data (I));
				end loop;
				pragma Assert (Ada.Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Text_IO.Close (File);
			end;
			pragma Assert (U.Size (Buffer) = Stream_Data'Length);
			U.Reset (Buffer);
			String'Read (U.Stream (Buffer), Data);
			pragma Assert (Data = Stream_Data);
		end Generic_Process;
	begin
		-- by Put
		declare
			procedure Process is new Generic_Process (Character, String, Ada.Text_IO.Overloaded_Put);
			procedure Process is new Generic_Process (Wide_Character, Wide_String, Ada.Text_IO.Overloaded_Put);
			procedure Process is new Generic_Process (Wide_Wide_Character, Wide_Wide_String, Ada.Text_IO.Overloaded_Put);
		begin
			-- writing a legal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_16);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_32);
			-- DBCS (SJIS)
			if Windows then
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_8);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_16);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_32);
			end if;
			-- writing an illegal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_16_As_UTF_8, Illegal_UTF_16);
		end;
	end;
	-- reading
	declare
		generic
			type Character_Type is (<>);
			type String_Type is array (Positive range <>) of Character_Type;
			with procedure Get (File : in Ada.Text_IO.File_Type; Item : out Character_Type);
		procedure Generic_Process (External : in Ada.IO_Modes.File_External_Spec; Stream_Data : in String; String_Data : in String_Type);
		procedure Generic_Process (External : in Ada.IO_Modes.File_External_Spec; Stream_Data : in String; String_Data : in String_Type) is
			Buffer : U.Buffer_Type;
		begin
			String'Write (U.Stream (Buffer), Stream_Data);
			U.Reset (Buffer);
			declare
				File : Ada.Text_IO.File_Type;
				Item : Character_Type;
			begin
				Ada.Text_IO.Text_Streams.Open (
					File,
					Ada.Text_IO.In_File,
					Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
					External => External);
				for I in String_Data'Range loop
					Get (File, Item);
					pragma Assert (Item = String_Data (I));
				end loop;
				pragma Assert (Ada.Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Text_IO.Close (File);
			end;
			-- with calling End_Of_*
			U.Reset (Buffer);
			declare
				File : Ada.Text_IO.File_Type;
				Item : Character_Type;
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
					Get (File, Item);
					pragma Assert (Item = String_Data (I));
				end loop;
				pragma Assert (Ada.Text_IO.End_Of_Line (File));
				pragma Assert (Ada.Text_IO.End_Of_Page (File));
				pragma Assert (Ada.Text_IO.End_Of_File (File));
				pragma Assert (Ada.Text_IO.Col (File) = Stream_Data'Length + 1);
				Ada.Text_IO.Close (File);
			end;
		end Generic_Process;
	begin
		-- by Get
		declare
			procedure Process is new Generic_Process (Character, String, Ada.Text_IO.Overloaded_Get);
			procedure Process is new Generic_Process (Wide_Character, Wide_String, Ada.Text_IO.Overloaded_Get);
			procedure Process is new Generic_Process (Wide_Wide_Character, Wide_Wide_String, Ada.Text_IO.Overloaded_Get);
		begin
			-- reading a legal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_16);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_32);
			-- DBCS (SJIS)
			if Windows then
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_8);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_16);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_32);
			end if;
			-- reading an illegal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8_As_UTF_16);
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8_As_UTF_32);
		end;
		-- by Get_Immediate
		declare
			procedure Process is new Generic_Process (Character, String, Ada.Text_IO.Overloaded_Get_Immediate);
			procedure Process is new Generic_Process (Wide_Character, Wide_String, Ada.Text_IO.Overloaded_Get_Immediate);
			procedure Process is new Generic_Process (Wide_Wide_Character, Wide_Wide_String, Ada.Text_IO.Overloaded_Get_Immediate);
		begin
			-- reading a legal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_16);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_32);
			-- DBCS (SJIS)
			if Windows then
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_8);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_16);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_32);
			end if;
			-- reading an illegal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8_As_UTF_16);
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8_As_UTF_32);
		end;
		-- by Look_Ahead and Get
		declare
			generic
				type Character_Type is (<>);
				with procedure Look_Ahead (File : in Ada.Text_IO.File_Type; Item : out Character_Type; End_Of_Line : out Boolean);
				with procedure Get (File : in Ada.Text_IO.File_Type; Item : out Character_Type);
			procedure Generic_Look_Ahead_And_Get (File : in Ada.Text_IO.File_Type; Item : out Character_Type);
			procedure Generic_Look_Ahead_And_Get (File : in Ada.Text_IO.File_Type; Item : out Character_Type) is
				End_Of_Line : Boolean;
				Item_Regotten : Character_Type;
			begin
				Look_Ahead (File, Item, End_Of_Line);
				pragma Assert (not End_Of_Line);
				Get (File, Item_Regotten);
				pragma Assert (Item_Regotten = Item);
			end Generic_Look_Ahead_And_Get;
			procedure Look_Ahead_And_Get is new Generic_Look_Ahead_And_Get (Character, Ada.Text_IO.Overloaded_Look_Ahead, Ada.Text_IO.Overloaded_Get);
			procedure Look_Ahead_And_Get is new Generic_Look_Ahead_And_Get (Wide_Character, Ada.Text_IO.Overloaded_Look_Ahead, Ada.Text_IO.Overloaded_Get);
			procedure Look_Ahead_And_Get is new Generic_Look_Ahead_And_Get (Wide_Wide_Character, Ada.Text_IO.Overloaded_Look_Ahead, Ada.Text_IO.Overloaded_Get);
			procedure Process is new Generic_Process (Character, String, Look_Ahead_And_Get);
			procedure Process is new Generic_Process (Wide_Character, Wide_String, Look_Ahead_And_Get);
			procedure Process is new Generic_Process (Wide_Wide_Character, Wide_Wide_String, Look_Ahead_And_Get);
		begin
			-- reading a legal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_16);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_32);
			-- DBCS (SJIS)
			if Windows then
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_8);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_16);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_32);
			end if;
			-- reading an illegal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8_As_UTF_16);
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8_As_UTF_32);
		end;
		-- by Look_Ahead and Skip_Ahead
		declare
			generic
				type Character_Type is (<>);
				with procedure Look_Ahead (File : in Ada.Text_IO.File_Type; Item : out Character_Type; End_Of_Line : out Boolean);
			procedure Generic_Look_Ahead_And_Get (File : in Ada.Text_IO.File_Type; Item : out Character_Type);
			procedure Generic_Look_Ahead_And_Get (File : in Ada.Text_IO.File_Type; Item : out Character_Type) is
				End_Of_Line : Boolean;
			begin
				Look_Ahead (File, Item, End_Of_Line);
				pragma Assert (not End_Of_Line);
				Ada.Text_IO.Skip_Ahead (File);
			end Generic_Look_Ahead_And_Get;
			procedure Look_Ahead_And_Get is new Generic_Look_Ahead_And_Get (Character, Ada.Text_IO.Overloaded_Look_Ahead);
			procedure Look_Ahead_And_Get is new Generic_Look_Ahead_And_Get (Wide_Character, Ada.Text_IO.Overloaded_Look_Ahead);
			procedure Look_Ahead_And_Get is new Generic_Look_Ahead_And_Get (Wide_Wide_Character, Ada.Text_IO.Overloaded_Look_Ahead);
			procedure Process is new Generic_Process (Character, String, Look_Ahead_And_Get);
			procedure Process is new Generic_Process (Wide_Character, Wide_String, Look_Ahead_And_Get);
			procedure Process is new Generic_Process (Wide_Wide_Character, Wide_Wide_String, Look_Ahead_And_Get);
		begin
			-- reading a legal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_16);
			Process (Ada.IO_Modes.UTF_8, Aegean_Number_One_In_UTF_8, Aegean_Number_One_In_UTF_32);
			-- DBCS (SJIS)
			if Windows then
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_8);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_16);
				Process (Ada.IO_Modes.Locale, Japanease_A_In_SJIS, Japanease_A_In_UTF_32);
			end if;
			-- reading an illegal sequence
			-- UTF-8
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8);
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8_As_UTF_16);
			Process (Ada.IO_Modes.UTF_8, Illegal_UTF_8, Illegal_UTF_8_As_UTF_32);
		end;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end textstream;
