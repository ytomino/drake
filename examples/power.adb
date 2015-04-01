with Ada;
procedure power is
	generic
		type T is range <>;
	procedure Generic_Signed_Test;
	procedure Generic_Signed_Test is
		function N (X : T; Y : Natural) return T is
			pragma Suppress (Overflow_Check);
		begin
			return X ** Y;
		end N;
		function P (X : T; Y : Natural) return T is
			pragma Unsuppress (Overflow_Check);
		begin
			return X ** Y;
		end P;
	begin
		pragma Assert (N (0, 0) = 1);
		pragma Assert (N (0, 1) = 0);
		pragma Assert (N (2, 3) = 8);
		pragma Assert (N (3, 3) = 27);
		pragma Assert (N (-3, 3) = -27);
		pragma Assert (P (0, 0) = 1);
		pragma Assert (P (0, 1) = 0);
		pragma Assert (P (2, 3) = 8);
		pragma Assert (P (3, 3) = 27);
		pragma Assert (P (-3, 3) = -27);
		null;
	end Generic_Signed_Test;
	generic
		type T is mod <>;
	procedure Generic_Unsigned_Test;
	procedure Generic_Unsigned_Test is
		function N (X : T; Y : Natural) return T is
			pragma Suppress (Overflow_Check);
			pragma Suppress (Range_Check);
		begin
			return X ** Y;
		end N;
		function P (X : T; Y : Natural) return T is
			pragma Unsuppress (Overflow_Check);
			pragma Suppress (Range_Check);
		begin
			return X ** Y;
		end P;
	begin
		pragma Assert (N (0, 0) = 1);
		pragma Assert (N (0, 1) = 0);
		pragma Assert (N (2, 3) = 8);
		pragma Assert (N (3, 3) = 27);
		pragma Assert (N (-3, 3) = -27);
		pragma Assert (P (0, 0) = 1);
		pragma Assert (P (0, 1) = 0);
		pragma Assert (P (2, 3) = 8);
		pragma Assert (P (3, 3) = 27);
		pragma Assert (P (-3, 3) = -27);
		null;
	end Generic_Unsigned_Test;
	procedure Integer_Test is new Generic_Signed_Test (Integer);
	procedure Long_Integer_Test is new Generic_Signed_Test (Long_Integer);
	procedure Long_Long_Integer_Test is new Generic_Signed_Test (Long_Long_Integer);
	type Unsigned is mod 2 ** Integer'Size;
	procedure Unsigned_Test is new Generic_Unsigned_Test (Unsigned);
	type Long_Unsigned is mod 2 ** Long_Integer'Size;
	procedure Long_Unsigned_Test is new Generic_Unsigned_Test (Long_Unsigned);
	type Long_Long_Unsigned is mod 2 ** Long_Long_Integer'Size;
	procedure Long_Long_Unsigned_Test is new Generic_Unsigned_Test (Long_Long_Unsigned);
	type Mod_100 is mod 100;
	procedure Mod_100_Test is new Generic_Unsigned_Test (Mod_100);
	type Mod_MNB is mod 2 ** 32 - 1; -- max non-binary
	procedure Mod_MNB_Test is new Generic_Unsigned_Test (Mod_MNB);
begin
	Integer_Test;
	Long_Integer_Test;
	Long_Long_Integer_Test;
	Unsigned_Test;
	Long_Unsigned_Test;
	Long_Long_Unsigned_Test;
	Mod_100_Test;
	Mod_MNB_Test;
	pragma Debug (Ada.Debug.Put ("OK"));
end power;
