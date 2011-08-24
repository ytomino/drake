with Ada.Directories;
with Ada.Directories.Information;
with Ada.Directories.Temporary;
with Ada.Permissions;
procedure directories is
	procedure Process (Directory_Entry : Ada.Directories.Directory_Entry_Type) is
	begin
		Ada.Debug.Put (Ada.Directories.Simple_Name (Directory_Entry));
		Ada.Debug.Put (Ada.Directories.Information.Owner (Directory_Entry));
		Ada.Debug.Put (Ada.Directories.Information.Group (Directory_Entry));
	end Process;
begin
	Ada.Debug.Put ("current user: " & Ada.Permissions.User_Name);
	Ada.Directories.Search (".", "*", Process => Process'Access);
	-- copy
	begin
		Ada.Directories.Copy_File ("%%%%NOTHING1%%%%", "%%%%NOTHING2%%%%");
		raise Program_Error;
	exception
		when Ada.Directories.Name_Error => null;
	end;
end directories;
