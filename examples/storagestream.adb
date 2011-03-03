with Ada.Streams.Overlay_Storage_IO;
with Ada.Streams.Buffer_Storage_IO;
procedure storagestream is
	Memory : Ada.Streams.Buffer_Storage_IO.Buffer;
	Stream : not null access Ada.Streams.Seekable_Stream_Type'Class :=
		Ada.Streams.Seekable_Stream_Type'Class (
			Ada.Streams.Buffer_Storage_IO.Stream (Memory).all)'Access;
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
end storagestream;
