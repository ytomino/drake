-- *** this line is for test ***
pragma Ada_2012;
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Credentials;
with Ada.Directories;
with Ada.Directories.Information;
with Ada.Directories.Temporary;
with Ada.Directories.Volumes;
with Ada.Text_IO;
procedure directories is
	use type Ada.Calendar.Time;
begin
	Ada.Debug.Put ("**** user");
	Ada.Debug.Put ("current user: " & Ada.Credentials.User_Name);
	-- iteration (closure style)
	Ada.Debug.Put ("**** iterate 1");
	declare
		First : Boolean := True;
		procedure Process (Directory_Entry : Ada.Directories.Directory_Entry_Type) is
		begin
			if First then
				Ada.Debug.Put (Ada.Directories.Information.Owner (Directory_Entry));
				Ada.Debug.Put (Ada.Directories.Information.Group (Directory_Entry));
				First := False;
			end if;
			Ada.Debug.Put (Ada.Directories.Simple_Name (Directory_Entry));
		end Process;
	begin
		Ada.Directories.Search (".", "*", Process => Process'Access);
	end;
	-- iteration (iterator style)
	Ada.Debug.Put ("**** iterate 2");
	declare
		Search : aliased Ada.Directories.Search_Type := Ada.Directories.Start_Search (".");
		Ite : Ada.Directories.Search_Iterator_Interfaces.Forward_Iterator'Class :=
			Ada.Directories.Iterate (Search);
		Position : Ada.Directories.Cursor := Ada.Directories.Search_Iterator_Interfaces.First (Ite);
	begin
		while Ada.Directories.Has_Element (Position) loop
			Ada.Debug.Put (Ada.Directories.Simple_Name (Search.Constant_Reference (Position).Element.all));
			Position := Ada.Directories.Search_Iterator_Interfaces.Next (Ite, Position);
		end loop;
	end;
	-- iteration (Ada 2012)
	Ada.Debug.Put ("**** iterate 3");
	for I of Ada.Directories.Start_Search (".") loop
		Ada.Debug.Put (Ada.Directories.Simple_Name (I));
	end loop;
	-- copy
	Ada.Debug.Put ("**** copy");
	begin
		Ada.Directories.Copy_File ("%%%%NOTHING1%%%%", "%%%%NOTHING2%%%%");
		raise Program_Error;
	exception
		when Ada.Directories.Name_Error => null;
	end;
	-- modification time
	Ada.Debug.Put ("**** modification time");
	declare
		Name : String := Ada.Command_Line.Command_Name & "-test";
		File : Ada.Text_IO.File_Type;
		The_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (1999, 7, 1);
	begin
		Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Name);
		Ada.Text_IO.Close (File);
		if abs (Ada.Directories.Modification_Time (Name) - Ada.Calendar.Clock) > 1.0 then
			raise Program_Error;
		end if;
		Ada.Directories.Set_Modification_Time (Name, The_Time);
		if abs (Ada.Directories.Modification_Time (Name) - The_Time) > 1.0 then
			raise Program_Error;
		end if;
		Ada.Directories.Delete_File (Name);
	end;
	-- symbolic link
	Ada.Debug.Put ("**** symbolic link");
	declare
		Source_Name : String := Ada.Directories.Full_Name ("directories.adb");
		Linked_Name : String := Ada.Command_Line.Command_Name & "-link";
		File : Ada.Text_IO.File_Type;
	begin
		Ada.Directories.Symbolic_Link (Source_Name, Linked_Name);
		Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Linked_Name);
		if Ada.Text_IO.Get_Line (File) /= "-- *** this line is for test ***" then
			raise Program_Error;
		end if;
		Ada.Text_IO.Close (File);
		if Ada.Directories.Information.Read_Symbolic_Link (Linked_Name) /= Source_Name then
			raise Program_Error;
		end if;
		Ada.Directories.Delete_File (Linked_Name);
	end;
	-- filesystem
	Ada.Debug.Put ("**** file system");
	declare
		FS : Ada.Directories.Volumes.File_System := Ada.Directories.Volumes.Where ("directories.adb");
	begin
		Ada.Debug.Put (Ada.Directories.File_Size'Image (Ada.Directories.Volumes.Size (FS)));
		Ada.Debug.Put (Ada.Directories.File_Size'Image (Ada.Directories.Volumes.Free_Space (FS)));
		Ada.Debug.Put (Ada.Directories.Volumes.Owner (FS));
		Ada.Debug.Put (Ada.Directories.Volumes.Format_Name (FS));
		Ada.Debug.Put (Ada.Directories.Volumes.Directory (FS));
		Ada.Debug.Put (Ada.Directories.Volumes.Device (FS));
	end;
	Ada.Debug.Put ("OK");
end directories;
