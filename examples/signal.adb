-- stack trace must be shown with gnatbind -E
with Ada.Unchecked_Conversion;
with Interfaces;
with System.Formatting;
with System.Machine_Code;
with System.Storage_Elements;
procedure signal is
	use type Interfaces.Unsigned_16;
	Bad : exception;
	CW : aliased Interfaces.Unsigned_16 := 0;
	type T is access Integer;
	function Cast is
		new Ada.Unchecked_Conversion (System.Storage_Elements.Integer_Address, T);
	A : T := Cast (12345678);
	procedure Access_Violation is
		pragma Suppress (All_Checks);
	begin
		A.all := 0; -- may cause SIGSEGV (segmentation fault)
	end Access_Violation;
	X : Integer := 10;
	Y : Integer := 20;
	procedure Divide_By_Zero is
		pragma Suppress (All_Checks);
	begin
		X := X / Y; -- may cause SIGFPE (floating point exception)
	end Divide_By_Zero;
begin
	System.Machine_Code.Asm ("fstcw (%0)",
		Inputs => System.Address'Asm_Input ("r", CW'Address),
		Volatile => True);
	CW := CW and not 16#001f#;
	System.Machine_Code.Asm ("fldcw %0",
		Inputs => Interfaces.Unsigned_16'Asm_Input ("m", CW),
		Volatile => True);
	CW := 0;
	System.Machine_Code.Asm ("fstcw (%0)",
		Inputs => System.Address'Asm_Input ("r", CW'Address),
		Volatile => True);
	declare
		S : String (1 .. 4);
		Last : Natural;
		Error : Boolean;
	begin
		System.Formatting.Image (
			System.Formatting.Word_Unsigned (CW),
			S,
			Last,
			16,
			Width => 4,
			Error => Error);
		Ada.Debug.Put (S);
	end;
	begin
		Access_Violation;
		raise Bad;
	exception
		when Program_Error =>
			null;
		when Storage_Error =>
			Ada.Debug.Put ("Storage_Error instead of Program_Error");
	end;
	Y := 0;
	begin
		Divide_By_Zero;
		raise Bad;
	exception
		when Constraint_Error =>
			null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end signal;
