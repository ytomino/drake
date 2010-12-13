-- stack trace must be shown with gnatbind -E
with Unchecked_Conversion; -- renamed ver
with System.Machine_Code;
with Interfaces;
procedure signal is
	use type Interfaces.Unsigned_16;
	CW : aliased Interfaces.Unsigned_16 := 0;
	procedure printf (S : String; W : Interfaces.Unsigned_16);
	procedure printf (S : String; double : Long_Float);
	pragma Import (C, printf);
	type T is access Integer;
	function Cast is new Unchecked_Conversion (Integer, T);
	A : T := Cast (12345678);
	X : Integer := 10;
	Y : Integer := 20;
	-- Z : Long_Float;
	function sqrt (d : Long_Float) return Long_Float;
	pragma Import (C, sqrt);
	procedure Deep is
		pragma Suppress (All_Checks);
	begin
		-- A.all := 0; -- may cause SIGSEGV (segmentation fault)
		X := X / Y; -- may cause SIGFPE (floating point exception)

		-- Z := sqrt (-1.0); -- OSX success ???
		-- printf ("%lf" & ASCII.LF & ASCII.NUL, Z);
	end Deep;
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
	printf ("%.4hx" & ASCII.LF & ASCII.NUL, CW);
	Y := 0;
	Deep;
end signal;
