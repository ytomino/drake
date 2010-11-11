with Ada.Text_IO;
procedure tty is
	C, D : Character;
	Avail : Boolean;
begin
	Ada.Text_IO.New_Page; -- clear screen
	Ada.Text_IO.Put ("push any key:");
	Ada.Text_IO.Get_Immediate (C);
	loop
		Ada.Text_IO.Get_Immediate (D, Avail);
		exit when Avail and then C = D;
		Ada.Text_IO.Put (C);
	end loop;
end tty;
