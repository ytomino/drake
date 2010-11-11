with Interfaces.C.Pointers;
procedure intf is
	type Unsigned_Long_Long is mod 2 ** Long_Long_Integer'Size;
	procedure sscanf (a1 : String; a2 : String; a3 : access Long_Long_Integer);
	procedure sscanf (a1 : String; a2 : String; a3 : access Unsigned_Long_Long);
	pragma Import (C, sscanf);
	procedure printf (a1 : String; a2 : Long_Long_Integer);
	procedure printf (a1 : String; a2 : Unsigned_Long_Long);
	procedure printf (a1 : String; a2 : Interfaces.Integer_32);
	procedure printf (a1 : String; a2 : Interfaces.C.ptrdiff_t);
	pragma Import (C, printf);
	N : aliased Long_Long_Integer;
	U : aliased Unsigned_Long_Long;
	I : aliased Interfaces.Integer_32 := 10;
	type A is array (Positive range <>) of aliased Integer;
	package Ps is new Interfaces.C.Pointers (
		Positive,
		Integer,
		A,
		Default_Terminator => 0);
	AO : aliased A (1 .. 10);
	E1 : not null Ps.Pointer := AO (3)'Access;
	E2 : not null Ps.Pointer := AO (7)'Access;
begin
	sscanf ("100", "%lld", N'Access);
	printf ("%lld" & Ascii.LF & Ascii.NUL, N * 2);
	sscanf ("100", "%llu", U'Access);
	printf ("%llu" & Ascii.LF & Ascii.NUL, U * 2);
	printf ("%d" & Ascii.LF & Ascii.NUL, Interfaces.sync_sub_and_fetch (I'Access, 1));
	printf ("%d" & Ascii.LF & Ascii.NUL, Ps."-" (E2, E1));
end intf;
