with Ada.Text_IO;
procedure tty1 is
	use type Ada.Text_IO.Count;
	C, D : Character;
	Avail : Boolean;
	Line_Length : Ada.Text_IO.Count := Ada.Text_IO.Line_Length;
	Start_Col, Current_Col : Ada.Text_IO.Count;
begin
	Ada.Text_IO.New_Page; -- clear screen
	Ada.Text_IO.Put ("push any key:");
	Ada.Text_IO.Get_Immediate (C);
	Start_Col := Ada.Text_IO.Col;
	Current_Col := Start_Col;
	loop
		Ada.Text_IO.Get_Immediate (D, Avail);
		exit when Avail and then C = D;
		Ada.Text_IO.Put (C);
		Current_Col := Current_Col + 1;
		if Current_Col = Line_Length then
			Ada.Text_IO.Set_Col (Start_Col);
			Current_Col := Start_Col;
		end if;
	end loop;
	Ada.Text_IO.New_Line;
end tty1;
