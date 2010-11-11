with Ada;
procedure exception3 is
begin
	raise Program_Error;
exception
	when E : Program_Error =>
		Ada.Debug.Put ("handled... and reraising...");
		raise;
end exception3;
