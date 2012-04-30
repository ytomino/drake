with Ada.Containers.Forward_Iterators;
with Ada.Directories;
with Ada.Text_IO.Iterators;
procedure input2forward is
	use Ada.Directories.Search_Iterator_Interfaces;
	package DII is new Ada.Containers.Forward_Iterators (
		Input_Cursor => Ada.Directories.Cursor,
		Has_Element => Ada.Directories.Has_Element,
		Input_Iterator => Ada.Directories.Search_Iterator_Interfaces.Forward_Iterator);
	use Ada.Text_IO.Iterators.Lines_Iterator_Interfaces;
	package TII is new Ada.Containers.Forward_Iterators (
		Input_Cursor => Ada.Text_IO.Iterators.Line_Cursor,
		Has_Element => Ada.Text_IO.Iterators.Has_Element,
		Input_Iterator => Ada.Text_IO.Iterators.Lines_Iterator_Interfaces.Forward_Iterator);
begin
	declare
		use Ada.Directories, Ada.Directories.Search_Iterator_Interfaces,
			DII, DII.Iterator_Interfaces;
		Search : aliased Search_Type := Start_Search (".", "*");
		Original_Ite : aliased Search_Iterator_Interfaces.Forward_Iterator'Class :=
			Iterate (Search);
		Ite : Iterator_Interfaces.Forward_Iterator'Class := Satisfy (Original_Ite);
		Pos : DII.Cursor := First (Ite);
	begin
		Ada.Debug.Put (">>>>");
		while Has_Element (Pos) loop
			Ada.Debug.Put (Simple_Name (Search.Constant_Reference (To (Pos).Element.all).Element.all));
			Pos := Next (Ite, Pos);
		end loop;
		Ada.Debug.Put ("<<<<");
	end;
--**--
	declare
		use Ada.Text_IO,
			Ada.Text_IO.Iterators, Ada.Text_IO.Iterators.Lines_Iterator_Interfaces,
			TII, TII.Iterator_Interfaces;
		File : aliased File_Type := Open (Mode => In_File, Name => "input2forward.adb"); -- this file
		Lines : aliased Lines_Type := Iterators.Lines (File);
		Original_Ite : aliased Lines_Iterator_Interfaces.Forward_Iterator'Class :=
			Iterate (Lines);
		Ite : Iterator_Interfaces.Forward_Iterator'Class := Satisfy (Original_Ite);
		Pos : TII.Cursor := First (Ite);
		J : TII.Cursor;
	begin
		Ada.Debug.Put (">>>>");
		while Has_Element (Pos) loop
			Ada.Debug.Put (Lines.Constant_Reference (To (Pos).Element.all).Element.all);
			if Lines.Constant_Reference (To (Pos).Element.all).Element.all = "--**--" then
				J := Pos;
			end if;
			Pos := Next (Ite, Pos);
		end loop;
		Ada.Debug.Put ("----");
		while Has_Element (J) loop
			Ada.Debug.Put (Lines.Constant_Reference (To (J).Element.all).Element.all);
			J := Next (Ite, J);
		end loop;
		Ada.Debug.Put ("<<<<");
	end;
end input2forward;
