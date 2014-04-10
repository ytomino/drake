with Ada;
with System.Unsigned_Types;
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
		pragma Assert (N (2, 3) = 8);
		pragma Assert (N (3, 3) = 27);
		pragma Assert (N (-3, 3) = -27);
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
		begin
			return X ** Y;
		end N;
		function P (X : T; Y : Natural) return T is
			pragma Unsuppress (Overflow_Check);
		begin
			return X ** Y;
		end P;
	begin
		pragma Assert (N (2, 3) = 8);
		pragma Assert (N (3, 3) = 27);
		pragma Assert (N (-3, 3) = -27);
		pragma Assert (P (2, 3) = 8);
		pragma Assert (P (3, 3) = 27);
		pragma Assert (P (-3, 3) = -27);
		null;
	end Generic_Unsigned_Test;
	procedure Integer_Test is new Generic_Signed_Test (Integer);
	procedure Long_Integer_Test is new Generic_Signed_Test (Long_Integer);
	procedure Long_Long_Integer_Test is new Generic_Signed_Test (Long_Long_Integer);
	procedure Unsigned_Test is new Generic_Unsigned_Test (System.Unsigned_Types.Unsigned);
	procedure Long_Unsigned_Test is new Generic_Unsigned_Test (System.Unsigned_Types.Long_Unsigned);
	procedure Long_Long_Unsigned_Test is new Generic_Unsigned_Test (System.Unsigned_Types.Long_Long_Unsigned);
	type Mod_100 is mod 100;
	procedure Mod_100_Test is new Generic_Unsigned_Test (Mod_100);
begin
	Integer_Test;
	Long_Integer_Test;
	Long_Long_Integer_Test;
	Unsigned_Test;
	Long_Unsigned_Test;
	Long_Long_Unsigned_Test;
	Mod_100_Test;
	pragma Debug (Ada.Debug.Put ("OK"));
end power;
