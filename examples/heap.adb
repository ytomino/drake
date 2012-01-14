with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Memory.Allocated_Size;
with System.Storage_Elements;
procedure heap is
	type Integer_Access is access all Integer;
	procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);
	package Conv is
		new System.Address_To_Named_Access_Conversions (Integer, Integer_Access);
	x, y : Integer_Access;
	procedure printf (F : in String; P : Integer_Access);
	procedure printf (F : in String; X : System.Storage_Elements.Storage_Count);
	pragma Import (C, printf);
begin
	x := new Integer;
	y := new Integer;
	printf ("%p" & ASCII.LF & ASCII.NUL, x);
	printf ("%d" & ASCII.LF & ASCII.NUL,
		System.Memory.Allocated_Size (Conv.To_Address (x)));
	printf ("%p" & ASCII.LF & ASCII.NUL, y);
	printf ("%d" & ASCII.LF & ASCII.NUL,
		System.Memory.Allocated_Size (Conv.To_Address (y)));
	Free (x);
	Free (y);
end heap;
