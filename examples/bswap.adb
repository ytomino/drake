-- Demonstration of the compiler-defined attribute: 'Scalar_Storage_Order
-- The expected output:
--   L: 1000000 0100000 1100000 0010000 1010000 0110000 1110000 0001000
--   B: 0000001 0000010 0000011 0000100 0000101 0000110 0000111 0001000
with Ada.Text_IO;
with System;
procedure bswap is
	type Unsigned_7 is mod 2 ** 7;
	type NA is array (1 .. 8) of Unsigned_7;
	for NA'Component_Size use 7;
	for NA'Size use 56;
	type LA is new NA; -- little endian
	for LA'Scalar_Storage_Order use System.Low_Order_First;
	type BA is new NA; -- big endian
	for BA'Scalar_Storage_Order use System.High_Order_First;
	N_Data : constant NA := (1, 2, 3, 4, 5, 6, 7, 8);
	L_Data : aliased LA := LA (N_Data);
	B_Data : aliased BA := BA (N_Data);
begin
	pragma Assert (LA'Size = 56);
	pragma Assert (BA'Size = 56);
	for I in NA'Range loop
		pragma Assert (L_Data (I) = B_Data (I));
		null;
	end loop;
	declare
		type Unsigned_1 is mod 2;
		for Unsigned_1'Size use 1;
		use Ada.Text_IO;
		package Unsigned_1_IO is new Modular_IO (Unsigned_1);
		use Unsigned_1_IO;
	begin
		-- dump L_Data
		declare
			type LR is array (0 .. NA'Size - 1) of Unsigned_1;
			for LR'Component_Size use 1;
			for LR'Size use 56;
			for LR'Scalar_Storage_Order use System.Low_Order_First;
			L_Repr : LR;
			for L_Repr'Address use L_Data'Address;
		begin
			Put ("L:");
			for I in LR'Range loop
				if I mod 7 = 0 then
					Put (' ');
				end if;
				Put (L_Repr (I), Width => 1);
			end loop;
			New_Line;
		end;
		-- dump B_Data
		declare
			type BR is array (0 .. NA'Size - 1) of Unsigned_1;
			for BR'Component_Size use 1;
			for BR'Size use 56;
			for BR'Scalar_Storage_Order use System.High_Order_First;
			B_Repr : BR;
			for B_Repr'Address use B_Data'Address;
		begin
			Put ("B:");
			for I in BR'Range loop
				if I mod 7 = 0 then
					Put (' ');
				end if;
				Put (B_Repr (I), Width => 1);
			end loop;
			New_Line;
		end;
	end;
end bswap;
