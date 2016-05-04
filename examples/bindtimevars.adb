with Ada.Bind_Time_Variables;
procedure bindtimevars is
begin
	for I in Ada.Bind_Time_Variables.Iterate loop
		Ada.Debug.Put (
			Ada.Bind_Time_Variables.Name (I)
			& " = "
			& Ada.Bind_Time_Variables.Value (I));
	end loop;
	pragma Assert (Ada.Bind_Time_Variables.Exists ("KEY1"));
	pragma Assert (Ada.Bind_Time_Variables.Value ("KEY1") = "VALUE1");
	pragma Assert (not Ada.Bind_Time_Variables.Exists ("NONEXISTENT"));
	pragma Debug (Ada.Debug.Put ("OK"));
end bindtimevars;
