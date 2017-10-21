with Ada.Processes;
with Ada.Text_IO;
with System.Program;
with System.Storage_Elements.Formatting;
with System.Unwind.Occurrences;
procedure btsym is
	procedure Report (
		X : System.Unwind.Exception_Occurrence;
		Where : String) is
	begin
		System.Unwind.Occurrences.Default_Report (X, Where);
		if X.Num_Tracebacks > 0 then
			declare
				T : constant String := Standard'Target_Name;
				Darwin : constant Boolean :=
					T (T'Last - 7 .. T'Last - 1) = "-darwin"
					or else T (T'Last - 8 .. T'Last - 2) = "-darwin";
				Executable : constant String := System.Program.Full_Name;
				Load_Address : constant System.Address :=
					System.Program.Load_Address;
				Command : String (
					1 ..
					256
						+ System.Unwind.Exception_Msg_Max_Length
						+ System.Unwind.Max_Tracebacks
							* (3 + (Standard'Address_Size + 3) / 4)
						+ Executable'Length);
				Last : Natural := 0;
				procedure Put (S : String);
				procedure Put (S : String) is
				begin
					Command (Last + 1 .. Last + S'Length) := S;
					Last := Last + S'Length;
				end Put;
				procedure Put (A : System.Address);
				procedure Put (A : System.Address) is
				begin
					Put ("0x");
					Put (System.Storage_Elements.Formatting.Image (A));
				end Put;
			begin
				if Darwin then
					Put ("atos -o ");
				else
					Put ("addr2line -e ");
				end if;
				Put (Executable);
				if Darwin then
					for I in T'Range loop
						if T (I) = '-' then
							Put (" -arch ");
							Put (T (T'First .. I - 1));
							exit;
						end if;
					end loop;
					Put (" -l ");
					Put (Load_Address);
				end if;
				for I in 1 .. X.Num_Tracebacks loop
					Put (" ");
					Put (X.Tracebacks (I));
				end loop;
				Command (Last + 1) := Character'Val (0);
				Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error.all);
				begin
					Ada.Processes.Shell (Command (1 .. Last));
				exception
					when Ada.Processes.Name_Error | Ada.Processes.Use_Error =>
						Ada.Text_IO.Put_Line (
							Ada.Text_IO.Standard_Error.all,
							"failed: " & Command (1 .. Last));
				end;
				--  Report_Hook is called just before abort, so it should flush.
				Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error.all);
			end;
		end if;
	end Report;
	-- level 2
	procedure Nest_2;
	pragma No_Inline (Nest_2);
	procedure Nest_2 is
	begin
		Ada.Debug.Put ("before raising");
		if Integer'Value ("1") = 1 then
			raise Program_Error; -- report backtrace as symbols
		end if;
		Ada.Debug.Put ("after raising");
	end Nest_2;
	-- level 1
	procedure Nest_1;
	pragma No_Inline (Nest_1);
	procedure Nest_1 is
	begin
		Ada.Debug.Put ("before Nest_2");
		Nest_2;
		Ada.Debug.Put ("after Nest_2");
	end Nest_1;
begin
	Ada.Debug.Put ("PROGRAM_ERROR is right.");
	System.Unwind.Occurrences.Report_Hook := Report'Unrestricted_Access;
	Ada.Debug.Put ("before Nest_1");
	Nest_1;
	Ada.Debug.Put ("after Nest_1");
end btsym;
