with Ada.Assertions;
procedure assert is
begin
	Ada.Debug.Put ("ADA.ASSERTIONS.ASSERTION_ERROR is right.");
	pragma Assert (False);
exception
	when Ada.Assertions.Assertion_Error =>
		Ada.Assertions.Assert (False);
end assert;
