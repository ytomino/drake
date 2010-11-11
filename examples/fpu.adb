with Ada;
with Interfaces;
with System.Machine_Code;
procedure fpu is
	use type Interfaces.Unsigned_16;
	CW : aliased Interfaces.Unsigned_16 := 0;
	procedure printf (S : String; W : Interfaces.Unsigned_16);
	pragma Import (C, printf);
begin
	System.Machine_Code.Asm ("fstcw (%0)",
		Inputs => System.Address'Asm_Input ("r", CW'Address),
		Volatile => True);
	printf ("%.4hx" & ASCII.LF & ASCII.NUL, CW);
	-- 0 : IM (Invalid-op Mask), 1 as masking exceptions
	-- 1 : DM (Denormal Mask)
	-- 2 : ZM (Zero-divide Mask)
	-- 3 : OM (Overflow Mask)
	-- 4 : UM (Underflow Mask)
	-- 5 : PM (Precision Mask)
	-- 6 .. 7 : ??
	-- 8 .. 9 : PC (Precision Control), expected to 2#11# as extended
	-- 10 .. 11 : RC (Rounding Control), 0 as rounding
	-- 12 : X (Infinity Control), no effect
	-- 13 .. 15 : ??
	if (CW and 16#0300#) = 16#0300# then
		Ada.Debug.Put ("ok, extended precision");
	end if;
end fpu;
