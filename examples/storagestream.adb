with Ada.Streams.Overlaps_Storage_IO;
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
	pragma Debug (Ada.Debug.Put ("OK"));
end storagestream;
