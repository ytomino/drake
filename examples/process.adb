pragma Check_Policy (Trace => Off);
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Processes;
with Ada.Streams.Stream_IO.Pipes;
with Ada.Text_IO.Text_Streams;
procedure process is
	use type Ada.Command_Line.Exit_Status;
	Target : constant String := Standard'Target_Name;
	In_Windows : constant Boolean :=
		Target (Target'Length - 6 .. Target'Last) = "mingw32";
begin
	declare -- making command line
		Command : Ada.Processes.Command_Type;
	begin
		Ada.Processes.Append (Command, "echo");
		Ada.Processes.Append (Command, "x");
		Ada.Processes.Append (Command, "y z");
		declare
			S1 : constant String := Ada.Processes.Image (Command);
			C2 : constant Ada.Processes.Command_Type := Ada.Processes.Value (S1);
			S2 : constant String := Ada.Processes.Image (C2);
		begin
			pragma Check (Trace, Ada.Debug.Put (S1));
			pragma Assert (S1 = S2);
			null;
		end;
	end;
	declare -- transfer command line
		Command : Ada.Processes.Command_Type;
	begin
		Ada.Processes.Append (Command, "echo");
		Ada.Processes.Append (Command, Ada.Command_Line.Iterate);
		pragma Check (Trace, Ada.Debug.Put (Ada.Processes.Image (Command)));
	end;
	declare -- shell
		Code : Ada.Command_Line.Exit_Status;
	begin
		-- ls
		if In_Windows then
			Ada.Processes.Shell ("cmd /c dir > nul", Code);
		else
			Ada.Processes.Shell ("ls > /dev/null", Code);
		end if;
		pragma Check (Trace, Ada.Debug.Put (Ada.Command_Line.Exit_Status'Image (Code)));
		pragma Assert (Code = 0);
		if In_Windows then
			Ada.Processes.Shell ("cmd /c dir $$$ > nul 2> nul", Code);
		else
			Ada.Processes.Shell ("ls @@@ 2> /dev/null", Code); -- is not existing
		end if;
		pragma Check (Trace, Ada.Debug.Put (Ada.Command_Line.Exit_Status'Image (Code)));
		pragma Assert (Code in 1 .. 2); -- GNU ls returns 2
		-- error case
		begin
			Ada.Processes.Shell ("acats 2> /dev/null", Code); -- dir
			raise Program_Error;
		exception
			when Ada.Processes.Name_Error =>
				null;
		end;
	end;
	declare -- spawn
		C : Ada.Processes.Process;
		Input_Reading, Input_Writing: Ada.Streams.Stream_IO.File_Type;
		Output_Reading, Output_Writing: Ada.Streams.Stream_IO.File_Type;
	begin
		-- env
		Ada.Environment_Variables.Set ("ahaha", "ufufu");
		Ada.Streams.Stream_IO.Pipes.Create (Output_Reading, Output_Writing);
		if In_Windows then
			Ada.Processes.Create (C, "C:\msys32\usr\bin\env.exe", Output => Output_Writing);
		else
			Ada.Processes.Create (C, "/usr/bin/env", Output => Output_Writing);
		end if;
		Ada.Streams.Stream_IO.Close (Output_Writing);
		Ada.Processes.Wait (C);
		declare
			File : Ada.Text_IO.File_Type;
			Success : Boolean := False;
		begin
			Ada.Text_IO.Text_Streams.Open (
				File,
				Ada.Text_IO.In_File,
				Ada.Streams.Stream_IO.Stream (Output_Reading));
			Reading : begin
				loop
					declare
						Line : constant String := Ada.Text_IO.Get_Line (File);
					begin
						pragma Check (Trace, Ada.Debug.Put (Line));
						if Line = "ahaha=ufufu" then
							Success := True;
						end if;
					end;
				end loop;
			exception
				when Ada.Text_IO.End_Error => null;
			end Reading;
			pragma Assert (Ada.Text_IO.End_Of_File (File));
			Ada.Text_IO.Close (File);
			pragma Assert (Success);
		end;
		Ada.Streams.Stream_IO.Close (Output_Reading);
		-- cat
		Ada.Streams.Stream_IO.Pipes.Create (Input_Reading, Input_Writing);
		Ada.Streams.Stream_IO.Pipes.Create (Output_Reading, Output_Writing);
		String'Write (Ada.Streams.Stream_IO.Stream (Input_Writing), "0123456789" & ASCII.LF);
		Ada.Streams.Stream_IO.Close (Input_Writing);
		if In_Windows then
			Ada.Processes.Create (C, "C:\msys32\usr\bin\cat.exe",
				Input => Input_Reading,
				Output => Output_Writing);
		else
			Ada.Processes.Create (C, "cat",
				Search_Path => True,
				Input => Input_Reading,
				Output => Output_Writing);
		end if;
		declare
			Terminated : Boolean;
		begin
			Ada.Processes.Wait_Immediate (C, Terminated => Terminated);
			pragma Assert (not Terminated);
		end;
		Ada.Streams.Stream_IO.Close (Input_Reading);
		Ada.Streams.Stream_IO.Close (Output_Writing);
		Ada.Processes.Wait (C);
		declare
			Buffer : String (1 .. 11);
		begin
			String'Read (Ada.Streams.Stream_IO.Stream (Output_Reading), Buffer);
			pragma Check (Trace, Ada.Debug.Put (Buffer));
			pragma Assert (Buffer = "0123456789" & ASCII.LF);
			pragma Assert (Ada.Streams.Stream_IO.End_Of_File (Output_Reading));
		end;
		Ada.Streams.Stream_IO.Close (Output_Reading);
		-- sleep and abort
		if In_Windows then
			Ada.Processes.Create (C, "C:\msys32\usr\bin\sleep.exe 2");
		else
			Ada.Processes.Create (C, "sleep 2", Search_Path => True);
		end if;
		Ada.Processes.Abort_Process (C);
		declare
			Code : Ada.Command_Line.Exit_Status;
		begin
			Ada.Processes.Wait (C, Code);
			pragma Check (Trace,
				Check => Ada.Debug.Put (Ada.Command_Line.Exit_Status'Image (Code)));
		end;
		-- error case
		begin
			Ada.Processes.Create (C, "acats"); -- dir
			Ada.Debug.Put ("fallthrough from Create");
			declare
				Code : Ada.Command_Line.Exit_Status;
			begin
				-- In Linux and glibc < 2.26, posix_spawn can't return the error.
				Ada.Processes.Wait (C, Code);
				if Code = 127 then
					raise Ada.Processes.Name_Error;
				end if;
				Ada.Debug.Put (Ada.Command_Line.Exit_Status'Image (Code));
			end;
			raise Program_Error;
		exception
			when Ada.Processes.Use_Error =>
				-- In Darwin, errno may be EACCES.
				null;
			when Ada.Processes.Name_Error =>
				-- In FreeBSD, errno may be ENOENT.
				-- In Windows, the executable attribute is missing.
				-- ERROR_FILE_NOT_FOUND may be returned.
				null;
		end;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end process;
