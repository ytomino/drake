with Ada;
procedure exception2 is
begin
	raise Program_Error;
exception
	when Program_Error =>
		Ada.Debug.Put ("handled!");
end exception2;
