-- secondary stack
with Ada.Exceptions;
procedure sstack is
	function Paren (S : String) return String is
	begin
		return "(" & S & ")";
	end Paren;
	procedure Heavy_Use (N : Integer) is
		S0 : String := (1 .. 16 * 1024 => <>);
		S1 : String := Paren (S0);
	begin
		if N < 50 then
			if N mod 7 /= 0 then
				Heavy_Use (N + 3);
			end if;
			if N mod 5 /= 0 then
				Heavy_Use (N + 3);
			end if;
		end if;
		pragma Assert (S1 (S1'First + 1 .. S1'Last - 1) = S0);
	end Heavy_Use;
begin
	Ada.Debug.Put (Paren ("[]"));
	Ada.Debug.Put (Paren ("{}"));
	Ada.Debug.Put (Ada.Exceptions.Exception_Name (Constraint_Error'Identity));
	Heavy_Use (1);
	Ada.Debug.Put ("OK");
end sstack;
