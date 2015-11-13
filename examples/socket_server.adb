--  telnet localhost 3776
with Ada.Streams.Stream_IO.Sockets;
procedure socket_server is
	Server : Ada.Streams.Stream_IO.Sockets.Listener :=
		Ada.Streams.Stream_IO.Sockets.Listen (3776);
begin
	pragma Debug (Ada.Debug.Put ("opened"));
	declare
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Sockets.Accept_Socket (Server, File);
		pragma Debug (Ada.Debug.Put ("connected"));
		while not Ada.Streams.Stream_IO.End_Of_File (File) loop
			declare
				Item : Ada.Streams.Stream_Element_Array (0 .. 0);
				Last : Ada.Streams.Stream_Element_Offset;
			begin
				Ada.Streams.Stream_IO.Read (File, Item, Last);
				Ada.Streams.Stream_IO.Write (File, Item (Item'First .. Last));
			end;
		end loop;
		Ada.Streams.Stream_IO.Close (File);
		pragma Debug (Ada.Debug.Put ("disconnected"));
	end;
	Ada.Streams.Stream_IO.Sockets.Close (Server);
	pragma Debug (Ada.Debug.Put ("closed"));
end socket_server;
