with Ada.Direct_IO;
with Ada.Sequential_IO;
with Ada.Storage_IO;
procedure typedio is
	package Character_IO is new Ada.Direct_IO (Character);
	package Duration_IO is new Ada.Sequential_IO (Duration);
	package Integer_IO is new Ada.Storage_IO (Integer);
begin
	null;
end typedio;
