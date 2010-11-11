with Ada.Unchecked_Deallocation;
procedure heap is
	type Integer_Access is access Integer;
	procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);
	x, y : Integer_Access;
	procedure printf (F : in String; P : Integer_Access);
	pragma Import (C, printf);
begin
	x := new Integer;
	y := new Integer;
	printf ("%p" & ASCII.LF & ASCII.NUL, x);
	printf ("%p" & ASCII.LF & ASCII.NUL, y);
	Free (x);
	Free (y);
end heap;
