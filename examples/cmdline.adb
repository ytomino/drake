pragma Ada_2012;
with Ada.Command_Line;
with Ada.Environment_Variables;
procedure cmdline is
	Count : Natural;
	Value_Test : Boolean := False;
	type Pair is record
		Name, Value : access String;
	end record;
	Rec : array (1 .. 255) of Pair;
	procedure Process (Name, Value : in String) is
	begin
		Count := Count + 1;
		Rec (Count).Name := new String'(Name);
		Rec (Count).Value := new String'(Value);
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
	-- iterate command line arguments
	Ada.Debug.Put (Ada.Command_Line.Command_Name);
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		Ada.Debug.Put (Ada.Command_Line.Argument (I));
	end loop;
	-- iterate environment variables by closure
	Ada.Debug.Put ("*** env(1) ***");
	Count := 0;
	Ada.Environment_Variables.Iterate (Process'Access);
	-- iterate environment variables by iterator
	Ada.Debug.Put ("*** env(2) ***");
	declare
		Count_2 : Natural := 0;
		Ite : Ada.Environment_Variables.Iterator_Interfaces.Forward_Iterator'Class :=
			Ada.Environment_Variables.Iterate;
		Pos : Ada.Environment_Variables.Cursor :=
			Ada.Environment_Variables.Iterator_Interfaces.First (Ite);
	begin
		while Ada.Environment_Variables.Has_Element (Pos) loop
			Count_2 := Count_2 + 1;
			pragma Assert (Ada.Environment_Variables.Name (Pos).Element.all = Rec (Count_2).Name.all);
			pragma Assert (Ada.Environment_Variables.Value (Pos).Element.all = Rec (Count_2).Value.all);
			Pos := Ada.Environment_Variables.Iterator_Interfaces.Next (Ite, Pos);
		end loop;
		pragma Assert (Count_2 = Count);
	end;
	-- iterate environment variables by user-defined loop of Ada 2012
--	Ada.Debug.Put ("*** env(3) ***");
--	declare
--		Count_3 : Natural := 0;
--	begin
--		for I in Ada.Environment_Variables.Iterate loop
--			Count_3 := Count_3 + 1;
--			pragma Assert (Ada.Environment_Variables.Name (I) = Rec (Count_3).Name.all);
--			pragma Assert (Ada.Environment_Variables.Value (I) = Rec (Count_3).Value.all);
--		end loop;
--		pragma Assert (Count_3 = Count);
--	end;
	-- modify environment variables
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
