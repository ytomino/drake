with Ada.Storage_Mapped_IO;
procedure mappedio is
	Mapping : Ada.Storage_Mapped_IO.Mapping;
begin
	Ada.Storage_Mapped_IO.Map (Mapping, Name => "mappedio.adb"); -- this file
	declare
		Image : String (1 .. Natural (Ada.Storage_Mapped_IO.Size (Mapping)));
		for Image'Address use Ada.Storage_Mapped_IO.Address (Mapping);
	begin
		Ada.Debug.Put (Image);
	end;
	Ada.Storage_Mapped_IO.Unmap (Mapping);
end mappedio;
