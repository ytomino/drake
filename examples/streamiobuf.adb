with Ada.Streams.Stream_IO.Naked;
with System.Native_IO;
procedure streamiobuf is
	use type Ada.Streams.Stream_Element;
	use type Ada.Streams.Stream_Element_Offset;
	package S renames Ada.Streams;
	package SIO renames Ada.Streams.Stream_IO;
	package NIO renames System.Native_IO;
	File : SIO.File_Type := SIO.Create;
	Handle : constant NIO.Handle_Type := SIO.Naked.Handle (File);
	Buffer_Size : constant S.Stream_Element_Count := NIO.Block_Size (Handle);
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
	-- writing, put into the buffer
	for I in Ada.Streams.Stream_Element'(16#41#) .. 16#5A# loop
		SIO.Write (File, (0 => I));
		pragma Assert (NIO.Index (Handle) = 1);
		pragma Assert (SIO.Index (File) = S.Stream_Element_Offset (I) - 16#41# + 2);
		pragma Assert (SIO.End_Of_File (File));
	end loop;
	-- writing, fill the buffer
	SIO.Write (File, (27 .. Buffer_Size => 0));
	pragma Assert (NIO.Index (Handle) = Buffer_Size + 1); -- flushed
	pragma Assert (SIO.Index (File) = Buffer_Size + 1);
	pragma Assert (SIO.End_Of_File (File));
	-- seek to half position
	SIO.Set_Index (File, Buffer_Size / 2 + 1);
	pragma Assert (NIO.Index (Handle) = Buffer_Size / 2 + 1);
	pragma Assert (SIO.Index (File) = Buffer_Size / 2 + 1);
	pragma Assert (not SIO.End_Of_File (File));
	-- write into half position
	SIO.Write (File, (0 => 16#21#));
	pragma Assert (NIO.Index (Handle) = Buffer_Size / 2 + 1);
	pragma Assert (SIO.Index (File) = Buffer_Size / 2 + 2);
	pragma Assert (not SIO.End_Of_File (File));
	-- reset for reading
	pragma Assert (NIO.Size (Handle) = Buffer_Size);
	pragma Assert (SIO.Size (File) = Buffer_Size);
	SIO.Reset (File, SIO.In_File);
	pragma Assert (NIO.Index (Handle) = 1);
	pragma Assert (SIO.Index (File) = 1);
	pragma Assert (not SIO.End_Of_File (File));
	-- reading, get from the buffer
	for I in Ada.Streams.Stream_Element'(16#41#) .. 16#5A# loop
		declare
			Item : S.Stream_Element_Array (0 .. 0);
			Last : S.Stream_Element_Offset;
		begin
			SIO.Read (File, Item, Last);
			pragma Assert (Last = 0);
			pragma Assert (Item (0) = I);
		end;
		pragma Assert (NIO.Index (Handle) = Buffer_Size + 1);
		pragma Assert (SIO.Index (File) = S.Stream_Element_Offset (I) - 16#41# + 2);
		pragma Assert (not SIO.End_Of_File (File));
	end loop;
	-- reading, get all remaining data in the buffer
	declare
		Item : S.Stream_Element_Array (27 .. Buffer_Size);
		Last : S.Stream_Element_Offset;
	begin
		SIO.Read (File, Item, Last);
		pragma Assert (Last = Item'Last);
	end;
	pragma Assert (NIO.Index (Handle) = Buffer_Size + 1);
	pragma Assert (SIO.Index (File) = Buffer_Size + 1);
	pragma Assert (SIO.End_Of_File (File));
	-- seek to half position
	SIO.Set_Index (File, Buffer_Size / 2 + 1);
	pragma Assert (NIO.Index (Handle) = Buffer_Size / 2 + 1);
	pragma Assert (SIO.Index (File) = Buffer_Size / 2 + 1);
	pragma Assert (not SIO.End_Of_File (File));
	-- reading, get from half position
	declare
		Item : S.Stream_Element_Array (0 .. 0);
		Last : S.Stream_Element_Offset;
	begin
		SIO.Read (File, Item, Last);
		pragma Assert (Last = 0);
		pragma Assert (Item (0) = 16#21#);
	end;
	pragma Assert (NIO.Index (Handle) = Buffer_Size + 1);
	pragma Assert (SIO.Index (File) = Buffer_Size / 2 + 2);
	pragma Assert (not SIO.End_Of_File (File));
	-- ok
	SIO.Close (File);
	pragma Debug (Ada.Debug.Put ("OK"));
end streamiobuf;
