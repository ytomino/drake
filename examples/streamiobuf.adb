with Ada.Numerics.Discrete_Random;
with Ada.Streams.Stream_IO.Naked;
with System.Native_IO;
procedure streamiobuf is
	use type Ada.Streams.Stream_Element;
	use type Ada.Streams.Stream_Element_Array;
	use type Ada.Streams.Stream_Element_Offset;
	package S renames Ada.Streams;
	package SIO renames Ada.Streams.Stream_IO;
	package NIO renames System.Native_IO;
	File : SIO.File_Type := SIO.Create;
	Handle : constant NIO.Handle_Type := SIO.Naked.Handle (File);
	Buffer_Size : constant S.Stream_Element_Count := NIO.Block_Size (Handle);
	Data : S.Stream_Element_Array (1 .. Buffer_Size * 9);
begin
	pragma Debug (Ada.Debug.Put (SIO.Name (File)));
	pragma Debug (Ada.Debug.Put ("buffer size:" & S.Stream_Element_Offset'Image (Buffer_Size)));
	pragma Assert (Buffer_Size > 1); -- ordinary file
	-- empty
	pragma Assert (NIO.Index (Handle) = 1);
	pragma Assert (SIO.Index (File) = 1);
	pragma Assert (SIO.End_Of_File (File));
	pragma Assert (NIO.Size (Handle) = 0);
	pragma Assert (SIO.Size (File) = 0);
	-- prepare the random data
	declare
		package SE_Random is new Ada.Numerics.Discrete_Random (S.Stream_Element);
		Gen : SE_Random.Generator := SE_Random.Initialize (16#ADA#);
	begin
		for I in Data'Range loop
			Data (I) := SE_Random.Random (Gen);
		end loop;
	end;
	-- writing
	declare
		Position : S.Stream_Element_Offset := 1;
		Native_Position : S.Stream_Element_Offset := 1;
		procedure Write (Rem_Size, Rounded_Size : S.Stream_Element_Count) is
			Old_Position : constant S.Stream_Element_Offset := Position;
		begin
			Position := Position + Rem_Size + Rounded_Size;
			Native_Position := Native_Position + Rounded_Size;
			SIO.Write (File, Data (Old_Position .. Position - 1));
			pragma Assert (NIO.Index (Handle) = Native_Position);
			pragma Assert (SIO.Index (File) = Position);
		end Write;
		procedure Set_Index (Index : S.Stream_Element_Offset) is
		begin
			Position := Index;
			Native_Position := Index;
			SIO.Set_Index (File, Position);
			pragma Assert (NIO.Index (Handle) = Native_Position);
			pragma Assert (SIO.Index (File) = Position);
		end Set_Index;
	begin
		for I in S.Stream_Element_Offset'(0) .. 1 loop
			-- from boundary to boundary
			Write (0, (I + 1) * Buffer_Size);
			pragma Assert (SIO.End_Of_File (File));
			-- from boundary to intermediate
			Write (Buffer_Size / 4, I * Buffer_Size);
			pragma Assert (SIO.End_Of_File (File));
			-- from intermediate to intermediate
			Write (Buffer_Size / 2, I * Buffer_Size);
			pragma Assert (SIO.End_Of_File (File));
			-- from intermediate to boundary
			Native_Position := Native_Position + Buffer_Size;
			Write (Buffer_Size - (Position - 1) rem Buffer_Size, I * Buffer_Size);
			pragma Assert (SIO.End_Of_File (File));
		end loop;
		pragma Assert (Position = Buffer_Size * 8 + 1);
		-- fill the page 9 by zero
		SIO.Write (File, (1 .. Buffer_Size => 0));
		-- from intermediate to boundary
		Set_Index (Buffer_Size / 4 * 3 + Buffer_Size * 8 + 1);
		Native_Position := Buffer_Size * 9 + 1;
		Write (Buffer_Size - (Position - 1) rem Buffer_Size, 0);
		pragma Assert (Position = Buffer_Size * 9 + 1);
		pragma Assert (SIO.End_Of_File (File));
		-- from intermediate to intermediate
		Set_Index (Buffer_Size / 4 + Buffer_Size * 8 + 1);
		Write (Buffer_Size / 2, 0);
		pragma Assert (Position = Buffer_Size / 4 * 3 + Buffer_Size * 8 + 1);
		pragma Assert (not SIO.End_Of_File (File));
		-- from boundary to intermediate
		Set_Index (Buffer_Size * 8 + 1);
		Write (Buffer_Size / 4, 0);
		pragma Assert (Position = Buffer_Size / 4 + Buffer_Size * 8 + 1);
		pragma Assert (not SIO.End_Of_File (File));
	end;
	-- reset for reading
	pragma Assert (NIO.Size (Handle) = Buffer_Size * 9);
	pragma Assert (SIO.Size (File) = Buffer_Size * 9);
	-- reading
	for EOF_Check in Boolean loop
		SIO.Reset (File, SIO.In_File);
		pragma Assert (NIO.Index (Handle) = 1);
		pragma Assert (SIO.Index (File) = 1);
		pragma Assert (not EOF_Check or else not SIO.End_Of_File (File));
		declare
			Position : S.Stream_Element_Offset := 1;
			Native_Position : S.Stream_Element_Offset := 1;
			procedure Read (Rem_Size, Rounded_Size : S.Stream_Element_Count) is
				Old_Position : constant S.Stream_Element_Offset := Position;
			begin
				Position := Position + Rem_Size + Rounded_Size;
				Native_Position := Native_Position + Rounded_Size;
				declare
					Read_Data : S.Stream_Element_Array (Old_Position .. Position - 1);
					Last : S.Stream_Element_Offset;
				begin
					SIO.Read (File, Read_Data, Last);
					pragma Assert (Last = Read_Data'Last);
					pragma Assert (Read_Data = Data (Read_Data'Range));
				end;
				pragma Assert (NIO.Index (Handle) = Native_Position);
				pragma Assert (SIO.Index (File) = Position);
			end Read;
			procedure Set_Index (Index : S.Stream_Element_Offset) is
			begin
				Position := Index;
				Native_Position := Index;
				SIO.Set_Index (File, Position);
				pragma Assert (NIO.Index (Handle) = Native_Position);
				pragma Assert (SIO.Index (File) = Position);
			end Set_Index;
		begin
			for I in S.Stream_Element_Offset'(0) .. 1 loop
				-- from boundary to boundary
				Read (0, (I + 1) * Buffer_Size);
				pragma Assert (not EOF_Check or else not SIO.End_Of_File (File));
				-- from boundary to intermediate
				Native_Position := Native_Position + Buffer_Size;
				Read (Buffer_Size / 4, I * Buffer_Size);
				pragma Assert (not EOF_Check or else not SIO.End_Of_File (File));
				-- from intermediate to intermediate
				Read (Buffer_Size / 2, I * Buffer_Size);
				pragma Assert (not EOF_Check or else not SIO.End_Of_File (File));
				-- from intermediate to boundary
				Read (Buffer_Size - (Position - 1) rem Buffer_Size, I * Buffer_Size);
				pragma Assert (not EOF_Check or else not SIO.End_Of_File (File));
			end loop;
			pragma Assert (Position = Buffer_Size * 8 + 1);
			-- from intermediate to boundary
			Set_Index (Buffer_Size / 4 * 3 + Buffer_Size * 8 + 1);
			Native_Position := Buffer_Size * 9 + 1;
			Read (Buffer_Size - (Position - 1) rem Buffer_Size, 0);
			pragma Assert (Position = Buffer_Size * 9 + 1);
			pragma Assert (not EOF_Check or else SIO.End_Of_File (File));
			-- from intermediate to intermediate
			Set_Index (Buffer_Size / 4 + Buffer_Size * 8 + 1);
			Native_Position := Buffer_Size * 9 + 1;
			Read (Buffer_Size / 2, 0);
			pragma Assert (Position = Buffer_Size / 4 * 3 + Buffer_Size * 8 + 1);
			pragma Assert (not EOF_Check or else not SIO.End_Of_File (File));
			-- from boundary to intermediate
			Set_Index (Buffer_Size * 8 + 1);
			Native_Position := Buffer_Size * 9 + 1;
			Read (Buffer_Size / 4, 0);
			pragma Assert (Position = Buffer_Size / 4 + Buffer_Size * 8 + 1);
			pragma Assert (not EOF_Check or else not SIO.End_Of_File (File));
		end;
	end loop;
	-- ok
	SIO.Close (File);
	pragma Debug (Ada.Debug.Put ("OK"));
end streamiobuf;
