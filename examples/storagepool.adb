with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Reallocation;
with System.Storage_Elements.Formatting;
with System.Storage_Pools.Unbounded;
procedure storagepool is
	use type System.Address;
	use type System.Storage_Elements.Storage_Offset;
	Verbose : constant Boolean := False;
begin
	Global : declare
		type A is access all Integer;
		type B is access all Integer;
		for B'Storage_Pool use A'Storage_Pool;
		procedure Free is new Ada.Unchecked_Deallocation (Integer, A);
		procedure Free is new Ada.Unchecked_Deallocation (Integer, B);
	begin
		declare
			X : A := new Integer'(100);
			Y : B := B (X);
		begin
			Free (Y);
		end;
		declare
			X : B := new Integer'(200);
			Y : A := A (X);
		begin
			Free (Y);
		end;
	end Global;
	Reallocation : declare
		type S is access String;
		procedure Reallocate is new Ada.Unchecked_Reallocation (Positive, Character, String, S);
		procedure Free is new Ada.Unchecked_Deallocation (String, S);
		X : S := new String'(257 => '[', 258 .. 511 => '-', 512 => ']');
	begin
		Reallocate (X, 1, 768);
		X (1 .. 256) := (others => '.');
		X (513 .. 678) := (others => '.');
		pragma Assert (X (257) = '[');
		pragma Assert (X (512) = ']');
		Reallocate (X, 257, 512);
		pragma Assert (X (257) = '[');
		pragma Assert (X (512) = ']');
		Reallocate (X, 129, 257);
		X (129 .. 256) := (others => '.');
		pragma Assert (X (257) = '[');
		Reallocate (X, 257, 512);
		X (258 .. 512) := (258 .. 511 => '-', 512 => ']');
		pragma Assert (X (257) = '[');
		Free (X);
	end Reallocation;
	Sized_And_Fixed : declare
		type T is access System.Address;
		for T'Storage_Size use (Standard'Address_Size / Standard'Storage_Unit) * 25;
		-- using System.Pool_Size
		-- Pool_Size => 100
		-- Elmt_Size => 4
		-- Alignment => 4
		procedure Free is new Ada.Unchecked_Deallocation (System.Address, T);
		A : array (1 .. 25) of T;
	begin
		for Trying in 1 .. 2 loop
			for I in A'Range loop
				A (I) := new System.Address'(System'To_Address (I));
				if Verbose then
					Ada.Debug.Put (System.Storage_Elements.Formatting.Image (A(I).all'Address));
				end if;
				pragma Assert (A (I).all'Address mod System.Address'Alignment = 0);
				for J in A'First .. I - 1 loop
					pragma Assert (A(I) /= A (J));
					null;
				end loop;
			end loop;
			begin
				A (1) := new System.Address; -- error
				raise Program_Error;
			exception
				when Storage_Error => null;
			end;
			for I in A'Range loop
				Free (A(I));
			end loop;
		end loop;
	end Sized_And_Fixed;
	Sized_And_Variable : declare
		type V is access String;
		for V'Storage_Size use 300;
		procedure Free is new Ada.Unchecked_Deallocation (String, V);
		A : array (1 .. 6) of V;
		First_Address : System.Address;
	begin
		A (1) := new String (1 .. 1);
		First_Address := A (1).all'Address;
		Free (A (1));
		for Trying in 1 .. 2 loop
			for I in A'Range loop
				A (I) := new String (1 .. I * 5);
				if Verbose then
					Ada.Debug.Put (System.Storage_Elements.Formatting.Image (A (I).all'Address));
				end if;
				for J in A'First .. I - 1 loop
					pragma Assert (A (J) /= A (I));
					null;
				end loop;
			end loop;
			begin
				A (1) := new String (1 .. 300); -- error
				raise Program_Error;
			exception
				when Storage_Error => null;
			end;
			pragma Assert (A (1).all'Address = First_Address);
			Free (A (6));
			Free (A (1));
			Free (A (2));
			Free (A (4));
			Free (A (3));
			Free (A (5));
		end loop;
		A (1) := new String (1 .. 100);
		pragma Assert (A (1).all'Address = First_Address);
		A (2) := new String (1 .. 1);
		Free (A (1));
		A (3) := new String (1 .. 5); -- reuse the area of A (1)
		pragma Assert (A (3).all'Address = First_Address);
		Free (A (3));
		A (4) := new String (1 .. 10); -- reuse the area of A (1)
		pragma Assert (A (4).all'Address = First_Address);
		A (5) := new String (1 .. 15); -- reuse the split area
		Free (A (4));
		Free (A (5));
		Free (A (2));
		A (1) := new String (1 .. 150);
		pragma Assert (A (1).all'Address = First_Address);
		Free (A (1));
	end Sized_And_Variable;
	Local : declare
		Pool : System.Storage_Pools.Unbounded.Unbounded_Pool;
		type T is access all Integer;
		for T'Storage_Pool use Pool;
		A : array (1 .. 2) of T;
	begin
		A (1) := new Integer'(1);
		A (2) := new Integer'(2);
	end Local;
	pragma Debug (Ada.Debug.Put ("OK"));
end storagepool;
