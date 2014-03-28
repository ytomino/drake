-- secondary stack
with Ada.Exceptions;
with System.Runtime_Context;
with System.Secondary_Stack;
with System.Storage_Elements;
with System.Termination;
with System.Unbounded_Stack_Allocators.Debug;
procedure sstack is
	procedure Dump_Secondary_Stack is
	begin
		System.Unbounded_Stack_Allocators.Debug.Dump (
			System.Runtime_Context.Get_Task_Local_Storage.Secondary_Stack'Access);
	end Dump_Secondary_Stack;
	function Paren (S : String) return String is
	begin
		return "(" & S & ")";
	end Paren;
	procedure Heavy_Use (N : Integer) is
		S0 : String := (1 .. 16 * 1024 => <>);
		S1 : String := Paren (S0);
	begin
--		Dump_Secondary_Stack;
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
	Dump_Secondary_Stack;
	declare
		M : System.Secondary_Stack.Mark_Id := 	System.Secondary_Stack.SS_Mark;
		A : System.Address;
	begin
		for I in System.Storage_Elements.Storage_Count'(1) .. 7 loop
			System.Secondary_Stack.SS_Allocate (A, I);
			System.Termination.Error_Put ("Allocated: ");
			System.Unbounded_Stack_Allocators.Debug.Error_Put (A);
			System.Termination.Error_New_Line;
			Dump_Secondary_Stack;
		end loop;
		System.Secondary_Stack.SS_Release (M);
	end;
	Dump_Secondary_Stack;
	Ada.Debug.Put (Paren ("[]"));
	Ada.Debug.Put (Paren ("{}"));
	Ada.Debug.Put (Ada.Exceptions.Exception_Name (Constraint_Error'Identity));
	Dump_Secondary_Stack;
	Heavy_Use (1);
	Dump_Secondary_Stack;
	pragma Debug (Ada.Debug.Put ("OK"));
end sstack;
