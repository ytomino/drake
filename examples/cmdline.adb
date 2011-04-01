with Ada.Command_Line;
with Ada.Environment_Variables;
procedure cmdline is
	procedure Process (Name, Value : in String) is
	begin
		Ada.Debug.Put (Name & "=" & Value);
	end Process;
begin
	Ada.Debug.Put (Ada.Command_Line.Command_Name);
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		Ada.Debug.Put (Ada.Command_Line.Argument (I));
	end loop;
	Ada.Debug.Put ("*** env(1) ***");
	Ada.Environment_Variables.Iterate (Process'Access);
	Ada.Debug.Put ("*** env(2) ***");
	declare
		use type Ada.Environment_Variables.Cursor;
		Ite : Ada.Environment_Variables.Iterator := Ada.Environment_Variables.Iterate;
		Pos : Ada.Environment_Variables.Cursor := Ada.Environment_Variables.First (Ite);
	begin
		while Pos /= Ada.Environment_Variables.No_Element loop
			Ada.Debug.Put (
				Ada.Environment_Variables.Constant_Reference (Pos).Name.all &
				"=" &
				Ada.Environment_Variables.Constant_Reference (Pos).Value.all);
			Pos := Ada.Environment_Variables.Next (Ite, Pos);
		end loop;
	end;
	Ada.Debug.Put ("*** clear ***");
	Ada.Environment_Variables.Clear;
	Ada.Environment_Variables.Set ("A", "B");
	Ada.Environment_Variables.Iterate (Process'Access);
	Ada.Debug.Put (Ada.Environment_Variables.Value ("A"));
end cmdline;
