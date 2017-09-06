with Ada.Streams.Unbounded_Storage_IO;
procedure storagestream is
	Memory : Ada.Streams.Unbounded_Storage_IO.Buffer_Type;
	Stream : not null access Ada.Streams.Seekable_Stream_Type'Class :=
		Ada.Streams.Seekable_Stream_Type'Class (
			Ada.Streams.Unbounded_Storage_IO.Stream (Memory).all)'Access;
begin
	String'Output (Stream, "ABCDEFG");
	Ada.Streams.Set_Index (Stream.all, 1);
	if String'Input (Stream) /= "ABCDEFG" then
		raise Program_Error;
	end if;
	Ada.Streams.Set_Index (Stream.all, 1);
	Wide_String'Output (Stream, "ABCDEFG");
	Ada.Streams.Set_Index (Stream.all, 1);
	if Wide_String'Input (Stream) /= "ABCDEFG" then
		raise Program_Error;
	end if;
	Ada.Streams.Set_Index (Stream.all, 1);
	Wide_Wide_String'Output (Stream, "ABCDEFG");
	Ada.Streams.Set_Index (Stream.all, 1);
	if Wide_Wide_String'Input (Stream) /= "ABCDEFG" then
		raise Program_Error;
	end if;
	declare -- do something with a copy
		N : Ada.Streams.Unbounded_Storage_IO.Buffer_Type := Memory;
		N_Stream : not null access Ada.Streams.Seekable_Stream_Type'Class :=
			Ada.Streams.Seekable_Stream_Type'Class (
				Ada.Streams.Unbounded_Storage_IO.Stream (N).all)'Access;
		Index : constant Ada.Streams.Stream_Element_Offset :=
			Ada.Streams.Index (Stream.all);
	begin
		Ada.Streams.Unbounded_Storage_IO.Reset (N);
		if Wide_Wide_String'Input (N_Stream) /= "ABCDEFG" then
			raise Program_Error;
		end if;
		String'Output (N_Stream, "RESERVEDAREA");
		String'Output (Stream, "ANOTHER");
		Ada.Streams.Set_Index (N_Stream.all, Index);
		if String'Input (N_Stream) /= "RESERVEDAREA" then
			raise Program_Error;
		end if;
		Ada.Streams.Set_Index (Stream.all, Index);
		if String'Input (Stream) /= "ANOTHER" then
			raise Program_Error;
		end if;
	end;
	declare -- write and read Buffer_Type itself
		Another : Ada.Streams.Unbounded_Storage_IO.Buffer_Type;
	begin
		Ada.Streams.Unbounded_Storage_IO.Buffer_Type'Write (
			Ada.Streams.Unbounded_Storage_IO.Stream (Another),
			Memory); -- save to another
		Ada.Streams.Unbounded_Storage_IO.Set_Size (Memory, 0); -- clear
		Ada.Streams.Unbounded_Storage_IO.Reset (Another);
		Ada.Streams.Unbounded_Storage_IO.Buffer_Type'Read (
			Ada.Streams.Unbounded_Storage_IO.Stream (Another),
			Memory); -- restore from another
		if Wide_Wide_String'Input (Stream) /= "ABCDEFG" then
			raise Program_Error;
		end if;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end storagestream;
