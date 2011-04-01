with Ada;
with System.Unsigned_Types;
procedure arrayop is
	use System.Unsigned_Types;
	generic
		type e is new Boolean;
		type a is array (Positive range <>) of e;
	procedure Generic_Test_Bits;
	procedure Generic_Test_Bits is
		False : constant e := e (Standard.False);
		True : constant e := e (Standard.True);
		ft : a := (e'Value ("False"), True);
		fftt : a := (e'Value ("False"), False, True, True);
		ftft : a := (e'Value ("False"), True, False, True);
		z2 : a (1 .. 2);
		z4 : a (1 .. 4);
	begin
		z2 := not ft; 	-- use s-boarop
		pragma Assert (z2 = a'(True, False));
		pragma Assert ((not ft) = a'(True, False)); -- inlined
		z4 := fftt and ftft; -- use s-boarop
		pragma Assert (z4 = a'(False, False, False, True));
		pragma Assert ((fftt and ftft) = a'(False, False, False, True)); -- inlined
		z4 := fftt or ftft; -- use s-boarop
		pragma Assert (z4 = a'(False, True, True, True));
		pragma Assert ((fftt or a'(False, True, False, True)) = a'(False, True, True, True)); -- inlined
		z4 := fftt xor ftft; -- use s-boarop
		pragma Assert (z4 = a'(False, True, True, False));
		pragma Assert ((fftt xor a'(False, True, False, True)) = a'(False, True, True, False)); -- inlined
		pragma Assert (Ada.Debug.Put (Integer'Image (a'Component_Size)));
		null;
	end Generic_Test_Bits;
	generic
		type e is (<>);
		type a is array (Positive range <>) of e;
	procedure Generic_Test_Comparison;
	procedure Generic_Test_Comparison is
	begin
		pragma Assert (a'(1 => e'First) = a'(1 => e'First));
		pragma Assert (a'(1 => e'First) /= a'(1 => e'Last));
		pragma Assert (a'(e'First, e'Last) < a'(e'Last, e'First));
		pragma Assert (a'(e'First, e'First) < a'(e'First, e'First, e'First));
		pragma Assert (a'(e'First, e'Last) > a'(e'First, e'First));
		pragma Assert (a'(e'First, e'First, e'First) > a'(e'First, e'First));
		pragma Assert (Ada.Debug.Put (Integer'Image (a'Component_Size)));
		null;
	end Generic_Test_Comparison;
	-- boolean
	type ba is array (Positive range <>) of Boolean;
	procedure tb_ba is new Generic_Test_Bits (Boolean, ba);
	pragma Debug (tb_ba);
	procedure tc_ba is new Generic_Test_Comparison (Boolean, ba);
	pragma Debug (tc_ba);
	-- 1
	type ba1 is array (Positive range <>) of Boolean;
	pragma Pack (ba1);
	procedure tb_ba1 is new Generic_Test_Bits (Boolean, ba1);
	pragma Debug (tb_ba1);
	procedure tc_ba1 is new Generic_Test_Comparison (Boolean, ba1);
	pragma Debug (tc_ba1);
	-- 2
	type ba2 is array (Positive range <>) of Boolean;
	for ba2'Component_Size use 2;
	procedure tb_ba2 is new Generic_Test_Bits (Boolean, ba2);
	pragma Debug (tb_ba2);
	procedure tc_ba2 is new Generic_Test_Comparison (Boolean, ba2);
	pragma Debug (tc_ba2);
	-- 4
	type ba4 is array (Positive range <>) of Boolean;
	for ba4'Component_Size use 4;
	procedure tb_ba4 is new Generic_Test_Bits (Boolean, ba4);
	pragma Debug (tb_ba4);
	procedure tc_ba4 is new Generic_Test_Comparison (Boolean, ba4);
	pragma Debug (tc_ba4);
	-- WordBool, see http://gcc.gnu.org/onlinedocs/gnat_rm/Effect-of-Convention-on-Representation.html
	type Boolean_16 is new Boolean;
	pragma Convention (C, Boolean_16);
	for Boolean_16'Size use 16;
	type ba16 is array (Positive range <>) of Boolean_16;
	procedure tb_ba16 is new Generic_Test_Bits (Boolean_16, ba16);
	pragma Debug (tb_ba16);
	procedure tc_ba16 is new Generic_Test_Comparison (Boolean_16, ba16);
	pragma Debug (tc_ba16);
	-- LongBool
	type Boolean_32 is new Boolean;
	pragma Convention (C, Boolean_32);
	for Boolean_32'Size use 32;
	type ba32 is array (Positive range <>) of Boolean_32;
	procedure tb_ba32 is new Generic_Test_Bits (Boolean_32, ba32);
	pragma Debug (tb_ba32);
	procedure tc_ba32 is new Generic_Test_Comparison (Boolean_32, ba32);
	pragma Debug (tc_ba32);
	-- integers
	type ssa is array (Positive range <>) of Short_Short_Integer;
	procedure tc_ssa is new Generic_Test_Comparison (Short_Short_Integer, ssa);
	pragma Debug (tc_ssa);
	type sa is array (Positive range <>) of Short_Integer;
	procedure tc_sa is new Generic_Test_Comparison (Short_Integer, sa);
	pragma Debug (tc_sa);
	type a is array (Positive range <>) of Integer;
	procedure tc_a is new Generic_Test_Comparison (Integer, a);
	pragma Debug (tc_a);
	type la is array (Positive range <>) of Long_Integer;
	procedure tc_la is new Generic_Test_Comparison (Long_Integer, la);
	pragma Debug (tc_la);
	type lla is array (Positive range <>) of Long_Long_Integer;
	procedure tc_lla is new Generic_Test_Comparison (Long_Long_Integer, lla);
	pragma Debug (tc_lla);
	-- unsigned integers
	type ssua is array (Positive range <>) of Short_Short_Unsigned;
	procedure tc_ssua is new Generic_Test_Comparison (Short_Short_Unsigned, ssua);
	pragma Debug (tc_ssua);
	type sua is array (Positive range <>) of Short_Unsigned;
	procedure tc_sua is new Generic_Test_Comparison (Short_Unsigned, sua);
	pragma Debug (tc_sua);
	type ua is array (Positive range <>) of Unsigned;
	procedure tc_ua is new Generic_Test_Comparison (Unsigned, ua);
	pragma Debug (tc_ua);
	type lua is array (Positive range <>) of Long_Unsigned;
	procedure tc_lua is new Generic_Test_Comparison (Long_Unsigned, lua);
	pragma Debug (tc_lua);
	type llua is array (Positive range <>) of Long_Long_Unsigned;
	procedure tc_llua is new Generic_Test_Comparison (Long_Long_Unsigned, llua);
	pragma Debug (tc_llua);
begin
	pragma Assert (Ada.Debug.Put ("OK"));
	null;
end arrayop;
