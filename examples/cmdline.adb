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
	Ada.Environment_Variables.Iterate (Process'Access);
	Ada.Debug.Put ("*** clear ***");
	Ada.Environment_Variables.Clear;
	Ada.Environment_Variables.Set ("A", "B");
	Ada.Environment_Variables.Iterate (Process'Access);
	Ada.Debug.Put (Ada.Environment_Variables.Value ("A"));
end cmdline;
