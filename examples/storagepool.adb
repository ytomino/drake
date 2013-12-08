with Ada.Unchecked_Deallocation;
--with System.Address_Image;
with System.Storage_Pools.Unbounded;
procedure storagepool is
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
	Sized_And_Fixed : declare
		type T is access System.Address;
		for T'Storage_Size use (System.Address'Size / Standard'Storage_Unit) * 25;
		-- using System.Pool_Size
		-- Pool_Size => 100
		-- Elmt_Size => 4
		-- Alignment => 4
		procedure Free is new Ada.Unchecked_Deallocation (System.Address, T);
		A : array (1 .. 25) of T;
	begin
		for I in A'Range loop
			A (I) := new System.Address'(System'To_Address (I));
--			Ada.Debug.Put (System.Address_Image (A(I).all'Address));
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
	end Sized_And_Fixed;
	Sized_And_Variable : declare
		type V is access String;
		for V'Storage_Size use 100;
		procedure Free is new Ada.Unchecked_Deallocation (String, V);
		A : array (1 .. 2) of V;
	begin
		A (1) := new String (1 .. 5);
--		Ada.Debug.Put (System.Address_Image (A (1).all'Address));
		A (2) := new String (1 .. 10);
--		Ada.Debug.Put (System.Address_Image (A (2).all'Address));
		pragma Assert (A (1) /= A (2));
		Free (A (1));
		Free (A (2));
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
