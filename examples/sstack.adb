-- secondary stack
with Ada.Exceptions;
with System.Secondary_Stack.Debug;
with System.Storage_Elements;
with System.Termination;
procedure sstack is
	function Paren (S : String) return String is
	begin
		return "(" & S & ")";
	end Paren;
	procedure Heavy_Use (N : Integer) is
		S0 : String := (1 .. 16 * 1024 => <>);
		S1 : String := Paren (S0);
	begin
--		System.Secondary_Stack.Debug.Dump;
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
	System.Secondary_Stack.Debug.Dump;
	declare
		M : System.Secondary_Stack.Mark_Id := 	System.Secondary_Stack.SS_Mark;
		A : System.Address;
	begin
		for I in System.Storage_Elements.Storage_Count'(1) .. 7 loop
			System.Secondary_Stack.SS_Allocate (A, I);
			System.Termination.Error_Put ("Allocated: ");
			System.Secondary_Stack.Debug.Error_Put (A);
			System.Termination.Error_New_Line;
			System.Secondary_Stack.Debug.Dump;
		end loop;
		System.Secondary_Stack.SS_Release (M);
	end;
	System.Secondary_Stack.Debug.Dump;
	Ada.Debug.Put (Paren ("[]"));
	Ada.Debug.Put (Paren ("{}"));
	Ada.Debug.Put (Ada.Exceptions.Exception_Name (Constraint_Error'Identity));
	System.Secondary_Stack.Debug.Dump;
	Heavy_Use (1);
	System.Secondary_Stack.Debug.Dump;
	pragma Debug (Ada.Debug.Put ("OK"));
end sstack;
