-- *** this line is for test ***
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Credentials;
with Ada.Directories.Information;
with Ada.Directories.Temporary;
with Ada.Directories.Volumes;
with Ada.Text_IO;
procedure directories is
	use type Ada.Calendar.Time;
	use type Ada.Directories.File_Kind;
	use type Ada.Directories.File_Size;
	use type Ada.Directories.Information.File_Id;
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
	-- iteration (AI12-0009-1, iterator)
	Ada.Debug.Put ("**** iterate 2");
	declare
		Listing : aliased Ada.Directories.Directory_Listing :=
			Ada.Directories.Entries (".");
		Ite : Ada.Directories.Directory_Iterators.Forward_Iterator'Class :=
			Ada.Directories.Iterate (Listing);
		Position : Ada.Directories.Cursor :=
			Ada.Directories.Directory_Iterators.First (Ite);
	begin
		while Ada.Directories.Has_Entry (Position) loop
			Ada.Debug.Put (
				Ada.Directories.Simple_Name (
					Listing.Constant_Reference (Position).Element.all));
			Position := Ada.Directories.Directory_Iterators.Next (Ite, Position);
		end loop;
	end;
	-- iteration (AI12-0009-1, generalized loop iteration)
	Ada.Debug.Put ("**** iterate 3");
	for I of Ada.Directories.Entries (".") loop
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
		Name : constant String := Ada.Directories.Temporary.Create_Temporary_File;
		File : Ada.Text_IO.File_Type;
		The_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (1999, 7, 1);
	begin
		Ada.Debug.Put (Name);
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
	-- getting information by Name versus by Directory_Entry
	Ada.Debug.Put ("**** information");
	declare
		Name : constant String := "directories.adb";
		Directory_Entry : Ada.Directories.Directory_Entry_Type := Ada.Directories.Get_Entry (Name);
	begin
		pragma Assert (Ada.Directories.Simple_Name (Name) = Ada.Directories.Simple_Name (Directory_Entry));
		pragma Assert (Ada.Directories.Kind (Name) = Ada.Directories.Kind (Directory_Entry));
		pragma Assert (Ada.Directories.Size (Name) = Ada.Directories.Size (Directory_Entry));
		pragma Assert (Ada.Directories.Modification_Time (Name) = Ada.Directories.Modification_Time (Directory_Entry));
		pragma Assert (Ada.Directories.Information.Identity (Name) = Ada.Directories.Information.Identity (Directory_Entry));
		null;
	end;
	-- user permissions
	Ada.Debug.Put ("**** permissions");
	declare
		UP : Ada.Directories.Information.User_Permission_Set_Type :=
			Ada.Directories.Information.User_Permission_Set ("directories.adb");
	begin
		for I in UP'Range loop
			Ada.Debug.Put (
				Ada.Directories.Information.User_Permission'Image (I)
				& " => "
				& Boolean'Image (UP (I)));
		end loop;
	end;
	-- symbolic link
	Ada.Debug.Put ("**** symbolic link");
	declare
		Source_Name : String := Ada.Directories.Full_Name ("directories.adb");
		Linked_Name : String := Ada.Directories.Temporary.Create_Temporary_File;
		File : Ada.Text_IO.File_Type;
	begin
		Ada.Debug.Put (Linked_Name);
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
		pragma Assert (Ada.Directories.Volumes.Is_Assigned (FS));
		Ada.Debug.Put (Ada.Directories.File_Size'Image (Ada.Directories.Volumes.Size (FS)));
		Ada.Debug.Put (Ada.Directories.File_Size'Image (Ada.Directories.Volumes.Free_Space (FS)));
		Ada.Debug.Put (Ada.Directories.Volumes.Owner (FS));
		Ada.Debug.Put (Ada.Directories.Volumes.Format_Name (FS));
		Ada.Debug.Put (Ada.Directories.Volumes.Directory (FS));
		Ada.Debug.Put (Ada.Directories.Volumes.Device (FS));
		Ada.Debug.Put ("case preserving: " & Boolean'Image (Ada.Directories.Volumes.Case_Preserving (FS)));
		Ada.Debug.Put ("case sensitive: " & Boolean'Image (Ada.Directories.Volumes.Case_Sensitive (FS)));
	end;
	Ada.Debug.Put ("OK");
end directories;
