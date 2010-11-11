with Ada.Assertions;
procedure assert is
begin
	pragma Assert (False);
	null;
exception
	when Ada.Assertions.Assertion_Error =>
		Ada.Assertions.Assert (False);
end assert;
