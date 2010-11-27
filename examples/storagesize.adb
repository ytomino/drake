with Ada.Unchecked_Deallocation;
with System.Address_Image;
procedure storagesize is
	type T is access Integer;
	for T'Storage_Size use (Integer'Size / Standard'Storage_Unit) * 25;
	-- using System.Pool_Size
	-- Pool_Size => 100
	-- Elmt_Size => 4
	-- Alignment => 4
	procedure Free is new Ada.Unchecked_Deallocation (Integer, T);
	A : array (1 .. 25) of T;
begin
	for I in A'Range loop
		A (I) := new Integer'(I);
		Ada.Debug.Put (System.Address_Image (A(I).all'Address));
	end loop;
	begin
		A (1) := new Integer; -- error
		raise Program_Error;
	exception
		when Storage_Error => null;
	end;
	for I in A'Range loop
		Free (A(I));
	end loop;
	declare
		type V is access String;
		for V'Storage_Size use 100;
		procedure Free is new Ada.Unchecked_Deallocation (String, V);
		A : array (1 .. 2) of V;
	begin
		A (1) := new String (1 .. 5);
		Ada.Debug.Put (System.Address_Image (A (1).all'Address));
		A (2) := new String (1 .. 10);
		Ada.Debug.Put (System.Address_Image (A (2).all'Address));
		Free (A (1));
		Free (A (2));
	end;
end storagesize;
