with Ada.Containers.Composites;
with Ada.Containers.Input_Iterators;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
procedure input is
	function End_Of_Entries is new Ada.Containers.Composites.XR_Not (
		Ada.Directories.Search_Type,
		Ada.Directories.More_Entries);
	package DII is new Ada.Containers.Input_Iterators (
		Element_Type => Ada.Directories.Directory_Entry_Type,
		File_Type => Ada.Directories.Search_Type,
		End_Of_File => End_Of_Entries,
		Get => Ada.Directories.Get_Next_Entry);
	-- Get (File : File_Type; ...) in series of Text_IO does not have "in out"
	procedure Get_Line is new Ada.Containers.Composites.XZ_Inout_1st (
		Ada.Text_IO.File_Type,
		Ada.Strings.Unbounded.Unbounded_String,
		Ada.Text_IO.Unbounded_IO.Get_Line);
	package TII is new Ada.Containers.Input_Iterators (
		Element_Type => Ada.Strings.Unbounded.Unbounded_String,
		File_Type => Ada.Text_IO.File_Type,
		End_Of_File => Ada.Text_IO.End_Of_File,
		Get => Get_Line);
begin
	declare
		use Ada.Directories, DII;
		Search : aliased Search_Type := Start_Search (".", "*");
		Ite : DII.Iterator := Iterate (Search'Access);
		Pos : DII.Cursor := First (Ite);
	begin
		Ada.Debug.Put (">>>>");
		while Pos /= No_Element loop
			Ada.Debug.Put (Simple_Name (Constant_Reference (Search, Pos).Element.all));
			Pos := Next (Ite, Pos);
		end loop;
		Ada.Debug.Put ("<<<<");
	end;
--**--
	declare
		use type Ada.Strings.Unbounded.Unbounded_String;
		use Ada.Text_IO, TII;
		File : aliased File_Type := Open (Mode => In_File, Name => "input.adb"); -- this file
		Ite : Iterator := Iterate (File'Access);
		Pos : Cursor := First (Ite);
		J : Cursor;
	begin
		Ada.Debug.Put (">>>>");
		while Pos /= No_Element loop
			Ada.Debug.Put (Ada.Strings.Unbounded.To_String (
				Constant_Reference (File, Pos).Element.all));
			if Constant_Reference (File, Pos).Element.all = "--**--" then
				J := Pos;
			end if;
			Pos := Next (Ite, Pos);
		end loop;
		Ada.Debug.Put ("----");
		while J /= No_Element loop
			Ada.Debug.Put (Ada.Strings.Unbounded.To_String (
				Constant_Reference (File, J).Element.all));
			J := Next (Ite, J);
		end loop;
		Ada.Debug.Put ("<<<<");
	end;
end input;
