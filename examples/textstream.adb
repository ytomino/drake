with Ada.IO_Modes;
with Ada.Streams.Stream_IO;
with Ada.Streams.Unbounded_Storage_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Wide_Text_IO.Text_Streams;
with Ada.Wide_Wide_Text_IO.Text_Streams;
procedure textstream is
	use type Ada.Streams.Stream_Element_Offset;
	package U renames Ada.Streams.Unbounded_Storage_IO;
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
begin
	-- writing a legal sequence
	declare
		Buffer : U.Buffer_Type;
		Data : String (1 .. 4);
	begin
		declare
			File : Ada.Text_IO.File_Type;
		begin
			Ada.Text_IO.Text_Streams.Open (
				File,
				Ada.Text_IO.Out_File,
				Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_8'Range loop
				Ada.Text_IO.Put (File, Aegean_Number_One_In_UTF_8 (I));
			end loop;
			Ada.Text_IO.Close (File);
			pragma Assert (U.Size (Buffer) = 4);
			U.Reset (Buffer);
			String'Read (U.Stream (Buffer), Data);
			pragma Assert (Data = Aegean_Number_One_In_UTF_8);
		end;
		U.Set_Size (Buffer, 0);
		declare
			File : Ada.Wide_Text_IO.File_Type;
		begin
			Ada.Wide_Text_IO.Text_Streams.Open (
				File,
				Ada.Wide_Text_IO.Out_File,
				Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_16'Range loop
				Ada.Wide_Text_IO.Put (File, Aegean_Number_One_In_UTF_16 (I));
			end loop;
			Ada.Wide_Text_IO.Close (File);
			pragma Assert (U.Size (Buffer) = 4);
			U.Reset (Buffer);
			String'Read (U.Stream (Buffer), Data);
			pragma Assert (Data = Aegean_Number_One_In_UTF_8);
		end;
		U.Set_Size (Buffer, 0);
		declare
			File : Ada.Wide_Wide_Text_IO.File_Type;
		begin
			Ada.Wide_Wide_Text_IO.Text_Streams.Open (
				File,
				Ada.Wide_Wide_Text_IO.Out_File,
				Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_32'Range loop
				Ada.Wide_Wide_Text_IO.Put (File, Aegean_Number_One_In_UTF_32 (I));
			end loop;
			Ada.Wide_Wide_Text_IO.Close (File);
			pragma Assert (U.Size (Buffer) = 4);
			U.Reset (Buffer);
			String'Read (U.Stream (Buffer), Data);
			pragma Assert (Data = Aegean_Number_One_In_UTF_8);
		end;
	end;
	-- reading a legal sequence
	declare
		Buffer : U.Buffer_Type;
	begin
		String'Write (U.Stream (Buffer), Aegean_Number_One_In_UTF_8);
		U.Reset (Buffer);
		declare
			File : Ada.Text_IO.File_Type;
			Item : Character;
		begin
			Ada.Text_IO.Text_Streams.Open (
				File,
				Ada.Text_IO.In_File,
				Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_8'Range loop
				Ada.Text_IO.Get (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_8 (I));
			end loop;
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
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_16'Range loop
				Ada.Wide_Text_IO.Get (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_16 (I));
			end loop;
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
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_32'Range loop
				Ada.Wide_Wide_Text_IO.Get (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_32 (I));
			end loop;
			Ada.Wide_Wide_Text_IO.Close (File);
		end;
	end;
	-- reading a legal sequence with calling End_Of_*
	declare
		Buffer : U.Buffer_Type;
	begin
		String'Write (U.Stream (Buffer), Aegean_Number_One_In_UTF_8);
		U.Reset (Buffer);
		declare
			File : Ada.Text_IO.File_Type;
			Item : Character;
		begin
			Ada.Text_IO.Text_Streams.Open (
				File,
				Ada.Text_IO.In_File,
				Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_8'Range loop
				pragma Assert (not Ada.Text_IO.End_Of_Line (File));
				pragma Assert (not Ada.Text_IO.End_Of_Page (File));
				pragma Assert (not Ada.Text_IO.End_Of_File (File));
				Ada.Text_IO.Get (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_8 (I));
			end loop;
			pragma Assert (Ada.Text_IO.End_Of_Line (File));
			pragma Assert (Ada.Text_IO.End_Of_Page (File));
			pragma Assert (Ada.Text_IO.End_Of_File (File));
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
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_16'Range loop
				pragma Assert (not Ada.Wide_Text_IO.End_Of_Line (File));
				pragma Assert (not Ada.Wide_Text_IO.End_Of_Page (File));
				pragma Assert (not Ada.Wide_Text_IO.End_Of_File (File));
				Ada.Wide_Text_IO.Get (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_16 (I));
			end loop;
			pragma Assert (Ada.Wide_Text_IO.End_Of_Line (File));
			pragma Assert (Ada.Wide_Text_IO.End_Of_Page (File));
			pragma Assert (Ada.Wide_Text_IO.End_Of_File (File));
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
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_32'Range loop
				pragma Assert (not Ada.Wide_Wide_Text_IO.End_Of_Line (File));
				pragma Assert (not Ada.Wide_Wide_Text_IO.End_Of_Page (File));
				pragma Assert (not Ada.Wide_Wide_Text_IO.End_Of_File (File));
				Ada.Wide_Wide_Text_IO.Get (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_32 (I));
			end loop;
			pragma Assert (Ada.Wide_Wide_Text_IO.End_Of_Line (File));
			pragma Assert (Ada.Wide_Wide_Text_IO.End_Of_Page (File));
			pragma Assert (Ada.Wide_Wide_Text_IO.End_Of_File (File));
			Ada.Wide_Wide_Text_IO.Close (File);
		end;
	end;
	-- reading a legal sequence by Get_Immediate
	declare
		Buffer : U.Buffer_Type;
	begin
		String'Write (U.Stream (Buffer), Aegean_Number_One_In_UTF_8);
		U.Reset (Buffer);
		declare
			File : Ada.Text_IO.File_Type;
			Item : Character;
		begin
			Ada.Text_IO.Text_Streams.Open (
				File,
				Ada.Text_IO.In_File,
				Ada.Text_IO.Text_Streams.Stream_Access (U.Stream (Buffer)),
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_8'Range loop
				Ada.Text_IO.Get_Immediate (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_8 (I));
			end loop;
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
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_16'Range loop
				Ada.Wide_Text_IO.Get_Immediate (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_16 (I));
			end loop;
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
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_32'Range loop
				Ada.Wide_Wide_Text_IO.Get_Immediate (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_32 (I));
			end loop;
			Ada.Wide_Wide_Text_IO.Close (File);
		end;
	end;
	-- reading a legal sequence with calling Look_Ahead
	declare
		Buffer : U.Buffer_Type;
	begin
		String'Write (U.Stream (Buffer), Aegean_Number_One_In_UTF_8);
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
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_8'Range loop
				Ada.Text_IO.Look_Ahead (File, Item, End_Of_Line);
				pragma Assert (not End_Of_Line);
				pragma Assert (Item = Aegean_Number_One_In_UTF_8 (I));
				Ada.Text_IO.Get (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_8 (I));
			end loop;
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
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_16'Range loop
				Ada.Wide_Text_IO.Look_Ahead (File, Item, End_Of_Line);
				pragma Assert (not End_Of_Line);
				pragma Assert (Item = Aegean_Number_One_In_UTF_16 (I));
				Ada.Wide_Text_IO.Get (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_16 (I));
			end loop;
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
				External => Ada.IO_Modes.UTF_8);
			for I in Aegean_Number_One_In_UTF_32'Range loop
				Ada.Wide_Wide_Text_IO.Look_Ahead (File, Item, End_Of_Line);
				pragma Assert (not End_Of_Line);
				pragma Assert (Item = Aegean_Number_One_In_UTF_32 (I));
				Ada.Wide_Wide_Text_IO.Get_Immediate (File, Item);
				pragma Assert (Item = Aegean_Number_One_In_UTF_32 (I));
			end loop;
			Ada.Wide_Wide_Text_IO.Close (File);
		end;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end textstream;
