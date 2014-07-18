-- find units unreferenced from examples
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Directories;
with Ada.Strings.Hash;
with Ada.Text_IO;
procedure ext_check is
	package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets (
		String,
		Hash => Ada.Strings.Hash,
		Equivalent_Elements => "=");
	procedure Process_Source_Dir (Path : in String; Units : in out String_Sets.Set) is
		S : Ada.Directories.Search_Type;
		E : Ada.Directories.Directory_Entry_Type;
	begin
		Ada.Directories.Start_Search (
			S,
			Path,
			"*.ads",
			Filter => ( -- ordinary file or symbolic link
				Ada.Directories.Ordinary_File | Ada.Directories.Special_File => True,
				others => False));
		while Ada.Directories.More_Entries (S) loop
			Ada.Directories.Get_Next_Entry (S, E);
			declare
				Name : constant String := Ada.Directories.Simple_Name (E);
			begin
				if Name (Name'First .. Name'First + 1) /= "c-" then -- Name'Length >= 4 since ".ads"
					String_Sets.Include (Units, Name);
				end if;
			end;
		end loop;
		Ada.Directories.End_Search (S);
	end Process_Source_Dir;
	procedure Process_Build_Dir (Path : in String; Used : in out String_Sets.Set) is
		S : Ada.Directories.Search_Type;
		E : Ada.Directories.Directory_Entry_Type;
	begin
		Ada.Directories.Start_Search (
			S,
			Path,
			"*.ali",
			Filter => (Ada.Directories.Ordinary_File => True, others => False));
		while Ada.Directories.More_Entries (S) loop
			Ada.Directories.Get_Next_Entry (S, E);
			declare
				F : Ada.Text_IO.File_Type;
			begin
				Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Name => Ada.Directories.Full_Name (E));
				while not Ada.Text_IO.End_Of_File (F) loop
					declare
						Line : constant String := Ada.Text_IO.Get_Line (F);
						First : Positive;
						Last : Natural;
					begin
						if Line'Length > 0
							and then Line (Line'First) = 'D'
							and then Line (Line'First + 1) = ' '
						then
							First := Line'First + 2; -- skip "D "
							Last := First;
							while Last < Line'Last and then Line (Last + 1) /= ASCII.HT loop
								Last := Last + 1;
							end loop;
							String_Sets.Include (Used, Line (First .. Last));
						end if;
					end;
				end loop;
				Ada.Text_IO.Close (F);
			end;
		end loop;
		Ada.Directories.End_Search (S);
	end Process_Build_Dir;
	function Is_Runtime_Unit (Path, Name : String) return Boolean is
	begin
		if Name (Name'First) = 's' then
			declare
				Result : Boolean;
				F : Ada.Text_IO.File_Type;
			begin
				Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Ada.Directories.Compose (Path, Name));
				declare
					S : constant String := Ada.Text_IO.Get_Line (F);
				begin
					pragma Assert (S = "pragma License (Unrestricted);");
					null;
				end;
				declare
					H : constant String := "--  runtime unit";
					S : constant String := Ada.Text_IO.Get_Line (F);
				begin
					Result := S'Length >= H'Length
						and then S (S'First .. S'First + H'Length - 1) = H;
				end;
				Ada.Text_IO.Close (F);
				return Result;
			end;
		else
			return False;
		end if;
	end Is_Runtime_Unit;
	Source_Dir : constant String := "bin/adainclude";
	Bin_Dir : constant String := "bin";
	Units, Used, Unused : aliased String_Sets.Set;
begin
	Process_Source_Dir (Source_Dir, Units);
	Process_Build_Dir (Bin_Dir, Used);
	Unused := String_Sets.Difference (Units, Used);
	declare
		I : String_Sets.Cursor := Unused.First;
	begin
		while String_Sets.Has_Element (I) loop
			declare
				Item : constant String := Unused.Constant_Reference (I).Element.all;
			begin
				if not Is_Runtime_Unit (Source_Dir, Item) then
					Ada.Text_IO.Put_Line (Item);
				end if;
			end;
			String_Sets.Next (I);
		end loop;
	end;
end ext_check;
