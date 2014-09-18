with Ada.Containers.Forward_Iterators;
with Ada.Directories;
with Ada.Text_IO.Iterators;
procedure input2forward is
begin
	declare
		use Ada.Directories;
		Search : aliased Search_Type := Start_Search (".", "*");
		package DII is new Ada.Containers.Forward_Iterators (
			Input_Cursor => Ada.Directories.Cursor,
			Has_Element => Ada.Directories.Has_Element,
			Input_Iterator_Interfaces => Ada.Directories.Search_Iterator_Interfaces,
			Input_Iterator => Ada.Directories.Iterate (Search),
			Element_Type => Ada.Directories.Directory_Entry_Type,
			Element => Search.Element);
		use DII, DII.Iterator_Interfaces;
		Ite : DII.Iterator_Interfaces.Forward_Iterator'Class := DII.Iterate;
		Pos : DII.Cursor := First (Ite);
		J : DII.Cursor;
	begin
		Ada.Debug.Put (">>>>");
		while Has_Element (Pos) loop
			Ada.Debug.Put (Ada.Directories.Simple_Name (DII.Constant_Reference (Pos).Element.all));
			if Ada.Directories.Simple_Name (DII.Constant_Reference (Pos).Element.all) = "input2forward.adb" then
				J := Pos;
			end if;
			Pos := Next (Ite, Pos);
		end loop;
		Ada.Debug.Put ("----");
		while Has_Element (J) loop
			Ada.Debug.Put (Ada.Directories.Simple_Name (DII.Constant_Reference (J).Element.all));
			J := Next (Ite, J);
		end loop;
		Ada.Debug.Put ("<<<<");
	end;
--**--
	declare
		use Ada.Text_IO, Ada.Text_IO.Iterators;
		File : aliased File_Type := Open (Mode => In_File, Name => "input2forward.adb"); -- this file
		Lines : aliased Lines_Type := Iterators.Lines (File);
		package TII is new Ada.Containers.Forward_Iterators (
			Input_Cursor => Ada.Text_IO.Iterators.Line_Cursor,
			Has_Element => Ada.Text_IO.Iterators.Has_Element,
			Input_Iterator_Interfaces => Ada.Text_IO.Iterators.Lines_Iterator_Interfaces,
			Input_Iterator => Ada.Text_IO.Iterators.Iterate (Lines),
			Element_Type => String,
			Element => Lines.Element);
		use TII, TII.Iterator_Interfaces;
		Ite : TII.Iterator_Interfaces.Forward_Iterator'Class := TII.Iterate;
		Pos : TII.Cursor := First (Ite);
		J : TII.Cursor;
	begin
		Ada.Debug.Put (">>>>");
		while Has_Element (Pos) loop
			Ada.Debug.Put (TII.Constant_Reference (Pos).Element.all);
			if TII.Constant_Reference (Pos).Element.all = "--**--" then
				J := Pos;
			end if;
			Pos := Next (Ite, Pos);
		end loop;
		Ada.Debug.Put ("----");
		while Has_Element (J) loop
			Ada.Debug.Put (TII.Constant_Reference (J).Element.all);
			J := Next (Ite, J);
		end loop;
		Ada.Debug.Put ("<<<<");
	end;
end input2forward;
