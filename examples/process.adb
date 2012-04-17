with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO.Pipes;
with Ada.Processes;
procedure process is
begin
	declare
		Code : Ada.Command_Line.Exit_Status;
	begin
		Ada.Processes.Shell ("ls", Code);
		Ada.Debug.Put (Ada.Command_Line.Exit_Status'Image (Code));
		Ada.Processes.Shell ("ls @@@", Code); -- is not existing
		Ada.Debug.Put (Ada.Command_Line.Exit_Status'Image (Code));
	end;
	declare
		C : Ada.Processes.Process;
	begin
		Ada.Environment_Variables.Clear;
		Ada.Environment_Variables.Set ("ahaha", "ufufu");
		Ada.Processes.Create (C, "/usr/bin/env");
		Ada.Processes.Create (C, "/bin/sh -c echo\ $ahaha");
		Ada.Processes.Wait (C);
	end;
end process;
