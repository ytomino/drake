-- please link with -lnosig.o
with Ada;
with Interfaces;
with System.Formatting;
with System.Machine_Code;
procedure nosignal is
	use type Interfaces.Unsigned_16;
	CW : aliased Interfaces.Unsigned_16 := 0;
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
	CW := CW and not 16#0004#; -- clear Zero divide Mask
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
	Ada.Debug.Put ("try to floating point exception");
	Y := 0;
	begin
		Divide_By_Zero;
		Ada.Debug.Put ("SIGFPE is ignored!!");
	exception
		when Constraint_Error =>
			Ada.Debug.Put ("SIGFPE is handled!!");
	end;
	Ada.Debug.Put ("failed.");
end nosignal;
