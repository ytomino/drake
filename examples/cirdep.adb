-- find circular dependency
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Strings.Functions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Iterators;
procedure cirdep is
	use type Ada.Strings.Unbounded.Unbounded_String;
	procedure Usage is
	begin
		Ada.Text_IO.Put_Line ("usage: cirdep --RTS=dir");
	end Usage;
	RTS_Dir : Ada.Strings.Unbounded.Unbounded_String;
begin
	-- arguments
	for I in Ada.Command_Line.Iterate loop
		declare
			Item : constant String := Ada.Command_Line.Argument (I);
		begin
			if Item'Length > 6
				and then Item (Item'First .. Item'First + 5) = "--RTS="
			then
				RTS_Dir := +Item (Item'First + 6 .. Item'Last);
			else
				Usage;
				return;
			end if;
		end;
	end loop;
	if RTS_Dir = Ada.Strings.Unbounded.Null_Unbounded_String then
		Usage;
		return;
	end if;
	declare
		package Unit_Sets is
			new Ada.Containers.Indefinite_Ordered_Sets (String);
		use type Unit_Sets.Set;
		package Unit_To_Unit_Sets_Maps is
			new Ada.Containers.Indefinite_Ordered_Maps (String, Unit_Sets.Set);
		Table : Unit_To_Unit_Sets_Maps.Map;
	begin
		-- reading
		for E of Ada.Directories.Entries (
			Ada.Directories.Compose (RTS_Dir.Constant_Reference, "adalib"),
			"*.ali")
		loop
			declare
				Name : constant String :=
					Ada.Directories.Base_Name (Ada.Directories.Simple_Name (E));
				The_Set : Unit_Sets.Set;
				File : Ada.Text_IO.File_Type :=
					Ada.Text_IO.Open (Ada.Text_IO.In_File, Ada.Directories.Full_Name (E));
			begin
				for Line of Ada.Text_IO.Iterators.Lines (File) loop
					if Line'Length >= 2
						and then Line (Line'First .. Line'First + 1) = "D "
					then
						declare
							Dep_Name : constant String :=
								Line (
									Line'First + 2 ..
									Ada.Strings.Functions.Index_Element (Line (Line'First + 2 .. Line'Last), '.') - 1);
						begin
							if Dep_Name /= Name then
								Unit_Sets.Include (The_Set, Dep_Name);
							end if;
						end;
					end if;
				end loop;
				Ada.Text_IO.Close (File);
				Unit_To_Unit_Sets_Maps.Insert (Table, Name, The_Set);
			end;
		end loop;
		-- removing separated units
		for I in Table.Iterate loop
			declare
				The_Set : Unit_Sets.Set renames Table.Reference (I);
				J : Unit_Sets.Cursor := The_Set.First;
			begin
				while Unit_Sets.Has_Element (J) loop
					declare
						Next : constant Unit_Sets.Cursor := Unit_Sets.Next (J);
						Dep_Name : constant String := The_Set.Constant_Reference (J);
					begin
						if not Unit_To_Unit_Sets_Maps.Contains (Table, Dep_Name) then
							Unit_Sets.Delete (The_Set, J);
						end if;
						J := Next;
					end;
				end loop;
			end;
		end loop;
		-- searching
		loop
			declare
				Changed : Boolean := False;
			begin
				for I in Table.Iterate loop
					declare
						Name : constant String := Unit_To_Unit_Sets_Maps.Key (I);
						The_Set : Unit_Sets.Set renames Table.Reference (I);
					begin
						for J in The_Set.Iterate loop
							declare
								Dep_Name : constant String := The_Set.Constant_Reference (J);
								The_Grandchildren : Unit_Sets.Set renames Table.Constant_Reference (Dep_Name);
							begin
								for K in The_Grandchildren.Iterate loop
									declare
										G_Dep_Name : constant String := The_Grandchildren.Constant_Reference (K);
									begin
										if G_Dep_Name /= Name
											and then not The_Set.Contains (G_Dep_Name)
										then
											Unit_Sets.Include (The_Set, G_Dep_Name);
											Changed := True;
										end if;
									end;
								end loop;
							end;
						end loop;
					end;
				end loop;
				exit when not Changed;
			end;
		end loop;
		-- detecting
		for I in Table.Iterate loop
			declare
				Name : constant String := Unit_To_Unit_Sets_Maps.Key (I);
				The_Set : Unit_Sets.Set renames Table.Constant_Reference (I);
			begin
				for J in The_Set.Iterate loop
					declare
						Dep_Name : constant String := The_Set.Constant_Reference (J);
					begin
						if Unit_Sets.Contains (Table.Constant_Reference (Dep_Name), Name) then
							Ada.Text_IO.Put (Name);
							Ada.Text_IO.Put (" <=> ");
							Ada.Text_IO.Put (The_Set.Constant_Reference (J));
							Ada.Text_IO.New_Line;
						end if;
					end;
				end loop;
			end;
		end loop;
	end;
end cirdep;
