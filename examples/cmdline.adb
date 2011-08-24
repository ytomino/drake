with Ada.Command_Line;
with Ada.Environment_Variables;
procedure cmdline is
	Count : Natural;
	Value_Test : Boolean := False;
	procedure Process (Name, Value : in String) is
	begin
		Count := Count + 1;
		Ada.Debug.Put (Name & "=" & Value);
		if Value_Test then
			if Name = "A" then
				pragma Assert (Value = "B");
				null;
			elsif Name = "C" then
				pragma Assert (Value = "");
				null;
			end if;
		end if;
	end Process;
begin
	Ada.Debug.Put (Ada.Command_Line.Command_Name);
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		Ada.Debug.Put (Ada.Command_Line.Argument (I));
	end loop;
	Ada.Debug.Put ("*** env(1) ***");
	Count := 0;
	Ada.Environment_Variables.Iterate (Process'Access);
	Ada.Debug.Put ("*** env(2) ***");
	declare
		use type Ada.Environment_Variables.Cursor;
		Ite : Ada.Environment_Variables.Iterator := Ada.Environment_Variables.Iterate;
		Pos : Ada.Environment_Variables.Cursor := Ada.Environment_Variables.First (Ite);
	begin
		while Ada.Environment_Variables.Has_Element (Pos) loop
			Count := Count - 1;
			Ada.Debug.Put (
				Ada.Environment_Variables.Name (Pos).Element.all
				& "="
				& Ada.Environment_Variables.Value (Pos).Element.all);
			Pos := Ada.Environment_Variables.Next (Ite, Pos);
		end loop;
	end;
	pragma Assert (Count = 0);
	Ada.Debug.Put ("*** clear ***");
	Ada.Environment_Variables.Clear;
	Ada.Environment_Variables.Set ("A", "B");
	Ada.Environment_Variables.Set ("C", "");
	Count := 0;
	Value_Test := True;
	Ada.Environment_Variables.Iterate (Process'Access);
	pragma Assert (Count = 2);
	pragma Assert (Ada.Environment_Variables.Value ("A") = "B");
	pragma Assert (Ada.Environment_Variables.Value ("C") = "");
	pragma Debug (Ada.Debug.Put ("OK"));
end cmdline;
