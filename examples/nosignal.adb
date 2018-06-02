-- Link with -lnosig.o
with Ada.Formatting;
with Interfaces;
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
		function Image is
			new Ada.Formatting.Modular_Image (
				Interfaces.Unsigned_16,
				Form => Ada.Formatting.Simple,
				Signs => Ada.Formatting.Triming_Unsign_Marks,
				Base => 16,
				Digits_Width => 4);
	begin
		Ada.Debug.Put (Image (CW));
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
