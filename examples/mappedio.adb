with Ada.Memory_Mapped_IO;
procedure mappedio is
	Mapping : Ada.Memory_Mapped_IO.Mapping;
begin
	Ada.Memory_Mapped_IO.Map (Mapping, Name => "mappedio.adb"); --  this file
	declare
		Image : String (1 .. Natural (Ada.Memory_Mapped_IO.Size (Mapping)));
		for Image'Address use Ada.Memory_Mapped_IO.Address (Mapping);
	begin
		Ada.Debug.Put (Image);
	end;
	Ada.Memory_Mapped_IO.Unmap (Mapping);
end mappedio;
