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
	-- 1-bit width unsigned integers
	type u1 is mod 2;
	for u1'Size use 1;
	type u1a is array (Positive range <>) of u1;
	for u1a'Component_Size use 1;
	procedure tc_u1a is new Generic_Test_Comparison (u1, u1a);
	pragma Debug (tc_u1a);
	-- 2-bit width unsigned integers
	type u2 is mod 2 ** 2;
	for u2'Size use 2;
	type u2a is array (Positive range <>) of u2;
	for u2a'Component_Size use 2;
	procedure tc_u2a is new Generic_Test_Comparison (u2, u2a);
	pragma Debug (tc_u2a);
	-- 3-bit width unsigned integers
	type u3 is mod 2 ** 3;
	for u3'Size use 3;
	type u3a is array (Positive range <>) of u3;
	for u3a'Component_Size use 3;
	procedure tc_u3a is new Generic_Test_Comparison (u3, u3a);
	pragma Debug (tc_u3a);
	-- 4-bit width unsigned integers
	type u4 is mod 2 ** 4;
	for u4'Size use 4;
	type u4a is array (Positive range <>) of u4;
	for u4a'Component_Size use 4;
	procedure tc_u4a is new Generic_Test_Comparison (u4, u4a);
	pragma Debug (tc_u4a);
	-- 5-bit width unsigned integers
	type u5 is mod 2 ** 5;
	for u5'Size use 5;
	type u5a is array (Positive range <>) of u5;
	for u5a'Component_Size use 5;
	procedure tc_u5a is new Generic_Test_Comparison (u5, u5a);
	pragma Debug (tc_u5a);
	-- 6-bit width unsigned integers
	type u6 is mod 2 ** 6;
	for u6'Size use 6;
	type u6a is array (Positive range <>) of u6;
	for u6a'Component_Size use 6;
	procedure tc_u6a is new Generic_Test_Comparison (u6, u6a);
	pragma Debug (tc_u6a);
	-- 7-bit width unsigned integers
	type u7 is mod 2 ** 7;
	for u7'Size use 7;
	type u7a is array (Positive range <>) of u7;
	for u7a'Component_Size use 7;
	procedure tc_u7a is new Generic_Test_Comparison (u7, u7a);
	pragma Debug (tc_u7a);
	-- 8-bit width unsigned integers
	type u8 is mod 2 ** 8;
	for u8'Size use 8;
	type u8a is array (Positive range <>) of u8;
	for u8a'Component_Size use 8;
	procedure tc_u8a is new Generic_Test_Comparison (u8, u8a);
	pragma Debug (tc_u8a);
	-- 9-bit width unsigned integers
	type u9 is mod 2 ** 9;
	for u9'Size use 9;
	type u9a is array (Positive range <>) of u9;
	for u9a'Component_Size use 9;
	procedure tc_u9a is new Generic_Test_Comparison (u9, u9a);
	pragma Debug (tc_u9a);
	-- 10-bit width unsigned integers
	type u10 is mod 2 ** 10;
	for u10'Size use 10;
	type u10a is array (Positive range <>) of u10;
	for u10a'Component_Size use 10;
	procedure tc_u10a is new Generic_Test_Comparison (u10, u10a);
	pragma Debug (tc_u10a);
	-- 11-bit width unsigned integers
	type u11 is mod 2 ** 11;
	for u11'Size use 11;
	type u11a is array (Positive range <>) of u11;
	for u11a'Component_Size use 11;
	procedure tc_u11a is new Generic_Test_Comparison (u11, u11a);
	pragma Debug (tc_u11a);
	-- 12-bit width unsigned integers
	type u12 is mod 2 ** 12;
	for u12'Size use 12;
	type u12a is array (Positive range <>) of u12;
	for u12a'Component_Size use 12;
	procedure tc_u12a is new Generic_Test_Comparison (u12, u12a);
	pragma Debug (tc_u12a);
	-- 13-bit width unsigned integers
	type u13 is mod 2 ** 13;
	for u13'Size use 13;
	type u13a is array (Positive range <>) of u13;
	for u13a'Component_Size use 13;
	procedure tc_u13a is new Generic_Test_Comparison (u13, u13a);
	pragma Debug (tc_u13a);
	-- 14-bit width unsigned integers
	type u14 is mod 2 ** 14;
	for u14'Size use 14;
	type u14a is array (Positive range <>) of u14;
	for u14a'Component_Size use 14;
	procedure tc_u14a is new Generic_Test_Comparison (u14, u14a);
	pragma Debug (tc_u14a);
	-- 15-bit width unsigned integers
	type u15 is mod 2 ** 15;
	for u15'Size use 15;
	type u15a is array (Positive range <>) of u15;
	for u15a'Component_Size use 15;
	procedure tc_u15a is new Generic_Test_Comparison (u15, u15a);
	pragma Debug (tc_u15a);
begin
	pragma Assert (Ada.Debug.Put ("OK"));
	null;
end arrayop;
