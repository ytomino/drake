-- document generator
with Ada.Containers.Limited_Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
procedure ext_doc is
	use type Ada.Strings.Unbounded.Unbounded_String;
	Prefix : constant String := "https://github.com/ytomino/drake/blob/master/";
	function Start_With (S, Prefix : String) return Boolean is
	begin
		return S'Length >= Prefix'Length and then S (S'First .. S'First + Prefix'Length - 1) = Prefix;
	end Start_With;
	Unknown_Unit_Kind_Error : exception;
	Unknown_Unit_Name_Error : exception;
	Parse_Error : exception;
	Extended_Style_Error : exception;
	Mismatch_Error : exception;
	type Unit_Kind is (Standard_Unit, Extended_Unit, Runtime_Unit, Implementation_Unit);
	type Unit_Contents is limited record
		File_Name : aliased Ada.Strings.Unbounded.Unbounded_String;
		Relative_Name : aliased Ada.Strings.Unbounded.Unbounded_String;
		Kind : Unit_Kind;
		Renamed : aliased Ada.Strings.Unbounded.Unbounded_String;
		Instantiation : aliased Ada.Strings.Unbounded.Unbounded_String;
		Reference : aliased Ada.Strings.Unbounded.Unbounded_String;
		Document : aliased Ada.Strings.Unbounded.Unbounded_String;
	end record;
	package Doc_Maps is new Ada.Containers.Limited_Ordered_Maps (String, Unit_Contents);
	Extendeds : aliased Doc_Maps.Map;
	procedure Process_Spec (Name : in String) is
		procedure Get_Unit_Name (
			Line : in String;
			Unit_Name : out Ada.Strings.Unbounded.Unbounded_String;
			Rest : out Ada.Strings.Unbounded.Unbounded_String)
		is
			F : Positive := Line'First;
			L : Integer;
		begin
			loop
				L := Ada.Strings.Fixed.Index (Line, ' ', From => F) - 1;
				if L < F and then Line (Line'Last) = ';' then
					L := Line'Last - 1;
				end if;
				if L < F then
					raise Unknown_Unit_Name_Error with Name & " """ & Line & """";
				end if;
				if Line (F .. L) = "package"
					or else Line (F .. L) = "procedure"
					or else Line (F .. L) = "function"
					or else Line (F .. L) = "generic"
					or else Line (F .. L) = "private"
				then
					F := Ada.Strings.Fixed.Index_Non_Blank (Line, From => L + 1);
				else
					Unit_Name := +Line (F .. L);
					Rest := +Line (L + 1 .. Line'Last);
					exit;
				end if;
			end loop;
		end Get_Unit_Name;
		File : Ada.Text_IO.File_Type;
		Unit_Name : aliased Ada.Strings.Unbounded.Unbounded_String;
		Kind : Unit_Kind;
		Renamed : Ada.Strings.Unbounded.Unbounded_String;
		Instantiation : Ada.Strings.Unbounded.Unbounded_String;
		Reference : Ada.Strings.Unbounded.Unbounded_String;
		Document : Ada.Strings.Unbounded.Unbounded_String;
		Rest_Of_Name_Line : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		-- Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Name);
		Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Name);
		if Start_With (Ada.Directories.Simple_Name (Name), "c-") then
			Kind := Implementation_Unit;
		else
			Detect_Kind : loop
				if Ada.Text_IO.End_Of_File (File) then
					raise Unknown_Unit_Kind_Error with Name;
				end if;
				declare
					Line : constant String := Ada.Text_IO.Get_Line (File);
				begin
					if Start_With (Line, "--") then
						if Start_With (Line, "--  Ada") or else Line = "--  separated and auto-loaded by compiler" then
							Kind := Standard_Unit;
							exit;
						elsif Start_With (Line, "--  extended ") then
							Kind := Extended_Unit;
							if Line /= "--  extended unit"
								and then not Start_With (Line, "--  extended unit specialized ")
								and then not Start_With (Line, "--  extended unit, ")
							then
								Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Name);
								Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "  " & Line);
							end if;
							exit;
						elsif Start_With (Line, "--  please see ") then
							Kind := Extended_Unit;
							Reference := +Line (Line'First + 15 .. Line'Last);
							exit;
						elsif Start_With (Line, "--  generic implementation of ") then
							Kind := Extended_Unit;
							Reference := +Line (Line'First + 30 .. Line'Last);
							exit;
						elsif Start_With (Line, "--  translated unit from ") then
							Kind := Extended_Unit;
							Reference := +Line (Line'First + 25 .. Line'Last);
							exit;
						elsif Start_With (Line, "--  runtime")
							or else Start_With (Line, "--  optional runtime")
						then
							Kind := Runtime_Unit;
							if Line /= "--  runtime unit"
								and then not Start_With (Line, "--  runtime unit ")
								and then Line /= "--  optional runtime unit"
							then
								Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Name);
								Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "  " & Line);
							end if;
							exit;
						elsif Start_With (Line, "--  implementation") then
							Kind := Implementation_Unit;
							exit;
						elsif Start_With (Line, "--  with") or else Start_With (Line, "--  diff") then
							null; -- skip
						else
							raise Unknown_Unit_Kind_Error with Name & " """ & Line & """";
						end if;
					elsif Start_With (Line, "package")
						or else Start_With (Line, "procedure")
						or else Start_With (Line, "function")
						or else Start_With (Line, "generic")
					then
						Kind := Standard_Unit;
						if Line /= "generic" then
							Get_Unit_Name (Line, Unit_Name, Rest_Of_Name_Line);
						end if;
						exit;
					end if;
				end;
			end loop Detect_Kind;
		end if;
		if Unit_Name.Is_Null then
			Detect_Name : loop
				if Ada.Text_IO.End_Of_File (File) then
					raise Unknown_Unit_Name_Error with Name;
				end if;
				declare
					Line : constant String := Ada.Text_IO.Get_Line (File);
				begin
					if Start_With (Line, "package")
						or else Start_With (Line, "procedure")
						or else Start_With (Line, "function")
						or else Start_With (Line, "private package")
						or else Start_With (Line, "private procedure")
						or else Start_With (Line, "private function")
						or else (Start_With (Line, "generic")
							and then Line /= "generic")
					then
						Get_Unit_Name (Line, Unit_Name, Rest_Of_Name_Line);
						exit;
					end if;
				end;
			end loop Detect_Name;
		end if;
		declare
			procedure Skip_Formal_Parameters is
				Closed : Boolean := False;
			begin
				loop
					declare
						Line : constant String := Ada.Text_IO.Get_Line (File);
					begin
						Closed := Closed or else Ada.Strings.Fixed.Index (Line, ')') > 0;
						exit when Closed and then Line (Line'Last) = ';';
					end;
				end loop;
			exception
				when Ada.Text_IO.End_Error =>
					raise Parse_Error with Name;
			end Skip_Formal_Parameters;
			function Get_Base (Line : String) return String is
				L : Natural;
				C : Integer := Ada.Strings.Fixed.Index (Line, " -- ");
			begin
				if C <= 0 then
					C := Line'Last + 1;
				end if;
				L := C - 1;
				if Line (L) = ';' then
					L := L - 1;
				elsif Line (L - 1 .. L) = " (" then
					L := L - 2;
					if Line (C - 1) /= ';' then
						Skip_Formal_Parameters;
					end if;
				end if;
				return Line (Line'First .. L);
			end Get_Base;
		begin
			Detect_Instantiantion_Or_Renamed : loop
				if Rest_Of_Name_Line = " is"
					or else Rest_Of_Name_Line.Is_Null
				then
					Rest_Of_Name_Line := +Ada.Text_IO.Get_Line (File);
				elsif Rest_Of_Name_Line = ";" then
					Rest_Of_Name_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
					exit;
				elsif Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, " (") then
					if Rest_Of_Name_Line.Element (Rest_Of_Name_Line.Length) /= ';' then
						Skip_Formal_Parameters;
					end if;
					Rest_Of_Name_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
					exit;
				elsif Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "   --")
					or else Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "   pragma")
					or else Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "--  pragma")
					or else Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "--  use")
					or else Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "   type")
					or else Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "--  type")
					or else Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "   subtype")
					or else Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "   procedure")
					or else Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "   function")
					or else Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "end")
					or else Ada.Strings.Unbounded.Index (Rest_Of_Name_Line, " : ") > 0
				then
					exit;
				elsif Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, " return ") then
					Renamed := +Get_Base (Rest_Of_Name_Line.Slice (1 + 8, Rest_Of_Name_Line.Length));
					Rest_Of_Name_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
					exit;
				elsif Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, " renames ") then
					Renamed := +Get_Base (Rest_Of_Name_Line.Slice (1 + 9, Rest_Of_Name_Line.Length));
					Rest_Of_Name_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
					exit;
				elsif Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, " is new ") then
					Instantiation := +Get_Base (Rest_Of_Name_Line.Slice (1 + 8, Rest_Of_Name_Line.Length));
					Rest_Of_Name_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
					exit;
				elsif Start_With (Rest_Of_Name_Line.Constant_Reference.Element.all, "   new ") then
					Instantiation := +Get_Base (Rest_Of_Name_Line.Slice (1 + 7, Rest_Of_Name_Line.Length));
					Rest_Of_Name_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
					exit;
				else
					raise Parse_Error with Name & " """ & Rest_Of_Name_Line.Constant_Reference.Element.all & """";
				end if;
			end loop Detect_Instantiantion_Or_Renamed;
		end;
		declare
			function Get_Next_Line return String is
			begin
				if not Rest_Of_Name_Line.Is_Null then
					return Result : String := Rest_Of_Name_Line.Constant_Reference.Element.all do
						Rest_Of_Name_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
					end return;
				else
					return Ada.Text_IO.Get_Line (File);
				end if;
			end Get_Next_Line;
		begin
			case Kind is
				when Standard_Unit =>
					declare
						Added_In_File : Boolean := False;
					begin
						while not Ada.Text_IO.End_Of_File (File) loop
							declare
								procedure Should_Be_Empty (S, Line : in String) is
								begin
									if S'Length > 0 then
										raise Extended_Style_Error with Name & " """ & Line & """";
									end if;
								end Should_Be_Empty;
								procedure Process (Block : in Boolean; Line : String; F : Integer) is
									type State_T is (Start, Comment, Code);
									State : State_T := Start;
									Indent : Natural := F - Line'First;
									Skip_F : Integer := 0;
									Comment_Only : Boolean := Block;
									Hiding : Boolean := False;
								begin
									if Added_In_File then
										Ada.Strings.Unbounded.Append (Document, ASCII.LF);
									end if;
									Added_In_File := True;
									while not Ada.Text_IO.End_Of_File (File) loop
										declare
											Ex_Line : constant String := Get_Next_Line;
											Ex_F : Integer := Ada.Strings.Fixed.Index_Non_Blank (Ex_Line);
										begin
											if (not Block or else Ex_Line /= "") and then Ex_F - Ex_Line'First < Indent and then not Start_With (Ex_Line, "--") then
												if State = Start then
													raise Extended_Style_Error with Name & " """ & Ex_Line & """";
												end if;
												exit;
											elsif Block and then Ex_F > 0 and then Start_With (Ex_Line (Ex_F .. Ex_Line'Last), "--  extended") then
												Should_Be_Empty (Ex_Line (Ex_F + 12 .. Ex_Line'Last), Ex_Line);
												Process (False, Ex_Line, Ex_F); -- switch to a normal extended section
												exit;
											end if;
											if State < Code and then Ex_F - Ex_Line'First = Indent and then Start_With (Ex_Line (Ex_F .. Ex_Line'Last), "--  ") then
												if Ex_Line (Ex_F + 4) = ' ' then
													Ex_F := Ada.Strings.Fixed.Index_Non_Blank (Ex_Line, From => Ex_F + 4);
													Ada.Strings.Unbounded.Append (Document, ' ' & Ex_Line (Ex_F .. Ex_Line'Last));
												else
													if State = Comment then
														Ada.Strings.Unbounded.Append (Document, ASCII.LF);
													end if;
													Ada.Strings.Unbounded.Append (Document, "| " & Ex_Line (Ex_F + 4 .. Ex_Line'Last));
												end if;
												State := Comment;
												if Comment_Only then
													Hiding := True;
												end if;
											elsif Ex_F > 0 and then Start_With (Ex_Line (Ex_F .. Ex_Line'Last), "pragma") then
												Skip_F := Ex_F;
												Comment_Only := False;
											elsif Start_With (Ex_Line, "--  diff")
												or else (Ex_F > 0 and then Start_With (Ex_Line (Ex_F .. Ex_Line'Last), "--  pragma"))
											then
												null;
											elsif Skip_F = 0 or else Ex_F <= Skip_F then
												Skip_F := 0;
												Comment_Only := False;
												if Hiding then
													if State = Comment then
														Ada.Strings.Unbounded.Append (Document, ASCII.LF);
													end if;
												else
													if State /= Code then
														if State = Comment then
															Ada.Strings.Unbounded.Append (Document, ASCII.LF & ASCII.LF);
														end if;
														Ada.Strings.Unbounded.Append (Document, ".. code-block:: ada" & ASCII.LF & ASCII.LF);
													end if;
													if Ex_Line'Length > 0 and then Ex_Line (Ex_Line'First) /= ' ' then
														Ada.Strings.Unbounded.Append (Document, ' ' & Ex_Line & ASCII.LF);
													else
														Ada.Strings.Unbounded.Append (Document, ' ' & Ex_Line (Ex_Line'First + Indent .. Ex_Line'Last) & ASCII.LF);
													end if;
												end if;
												State := Code;
											end if;
										end;
									end loop;
									if State = Comment then
										Ada.Strings.Unbounded.Append (Document, ASCII.LF);
									end if;
								end Process;
								Line : constant String := Get_Next_Line;
								F : Integer := Ada.Strings.Fixed.Index_Non_Blank (Line);
							begin
								if F > 0 then
									if Start_With (Line (F .. Line'Last), "--  extended from here") then
										Should_Be_Empty (Line (F + 22 .. Line'Last), Line);
										Process (True, Line, F);
									elsif Start_With (Line (F .. Line'Last), "--  extended") then
										Should_Be_Empty (Line (F + 12 .. Line'Last), Line);
										Process (False, Line, F);
									elsif Start_With (Line (F .. Line'Last), "--  modified") then
										Should_Be_Empty (Line (F + 12 .. Line'Last), Line);
										Process (False, Line, F);
									elsif Ada.Strings.Fixed.Index (Line, "-- extended") > 0
										or else Ada.Strings.Fixed.Index (Line, "--  extended") > 0
									then
										raise Extended_Style_Error with Name & " """ & Line & """";
									end if;
								end if;
							end;
						end loop;
					end;
				when Extended_Unit =>
					while not Ada.Text_IO.End_Of_File (File) loop
						declare
							Line : constant String := Get_Next_Line;
							F : Integer;
						begin
							F := Ada.Strings.Fixed.Index_Non_Blank (Line);
							if F > 0 and then Start_With (Line (F .. Line'Last), "--  ")
								and then not Start_With (Line (F .. Line'Last), "--  pragma")
							then
								F := F + 4;
								if Line (F) = ' ' then
									Ada.Strings.Unbounded.Append (Document, ' '); -- single space
									F := Ada.Strings.Fixed.Index_Non_Blank (Line, From => F);
								else
									if not Document.Is_Null then
										Ada.Strings.Unbounded.Append (Document, ASCII.LF);
									end if;
									Ada.Strings.Unbounded.Append (Document, "| ");
								end if;
								Ada.Strings.Unbounded.Append (Document, Line (F .. Line'Last));
							else
								exit;
							end if;
						end;
					end loop;
					if not Document.Is_Null then
						Ada.Strings.Unbounded.Append (Document, ASCII.LF);
					end if;
				when Runtime_Unit | Implementation_Unit =>
					null;
			end case;
		end;
		Ada.Text_IO.Close (File);
		if Extendeds.Contains (Unit_Name.Constant_Reference.Element.all) then
			Check : declare
				Position : Doc_Maps.Cursor := Extendeds.Find (Unit_Name.Constant_Reference.Element.all);
			begin
				if Extendeds.Constant_Reference (Position).Element.File_Name /= Ada.Directories.Simple_Name (Name)
					or else Extendeds.Constant_Reference (Position).Element.Kind /= Kind
					or else Extendeds.Constant_Reference (Position).Element.Renamed /= Renamed
					or else Extendeds.Constant_Reference (Position).Element.Instantiation /= Instantiation
					or else Extendeds.Constant_Reference (Position).Element.Reference /= Reference
					or else (Extendeds.Constant_Reference (Position).Element.Document /= Document
						and then Unit_Name /= "Interfaces.C" -- wchar_t is different
						and then Unit_Name /= "Ada.Directories.Information")
				then
					raise Mismatch_Error with Name;
				end if;
			end Check;
		else
			Insert : declare
				function New_Key return String is
				begin
					return Unit_Name.Constant_Reference.Element.all;
				end New_Key;
				function New_Element return Unit_Contents is
				begin
					pragma Assert (Name (Name'First .. Name'First + 2) = "../");
					return (
						File_Name => +Ada.Directories.Simple_Name (Name),
						Relative_Name => +Name (Name'First + 3 .. Name'Last),
						Kind => Kind,
						Renamed => Renamed,
						Instantiation => Instantiation,
						Reference => Reference,
						Document => Document);
				end New_Element;
			begin
				if Kind = Extended_Unit or else not Document.Is_Null then
					Doc_Maps.Insert (Extendeds, New_Key'Access, New_Element'Access);
				end if;
			end Insert;
		end if;
	end Process_Spec;
	procedure Process_Dir (Path : in String) is
		S : Ada.Directories.Search_Type;
		E : Ada.Directories.Directory_Entry_Type;
	begin
		Ada.Directories.Start_Search (
			S,
			Path,
			"*.ads",
			Filter => (Ada.Directories.Ordinary_File => True, others => False));
		while Ada.Directories.More_Entries (S) loop
			Ada.Directories.Get_Next_Entry (S, E);
			Process_Spec (Ada.Directories.Compose (Path, Ada.Directories.Simple_Name (E)));
		end loop;
		Ada.Directories.End_Search (S);
		Ada.Directories.Start_Search (
			S,
			Path,
			Filter => (Ada.Directories.Directory => True, others => False));
		while Ada.Directories.More_Entries (S) loop
			Ada.Directories.Get_Next_Entry (S, E);
			Process_Dir (Ada.Directories.Compose (Path, Ada.Directories.Simple_Name (E)));
		end loop;
		Ada.Directories.End_Search (S);
	end Process_Dir;
begin
	Process_Dir ("../source");
	Ada.Text_IO.Put_Line (".. contents::");
	Ada.Text_IO.New_Line;
	declare
		procedure Output_Unit (I : in Doc_Maps.Cursor) is
			Unit_Name : String renames Doc_Maps.Key (I).Element.all;
			Contents : Unit_Contents renames Extendeds.Constant_Reference (I).Element.all;
		begin
			Ada.Text_IO.Put_Line (Unit_Name);
			Ada.Text_IO.Put_Line ((1 .. Unit_Name'Length => '-'));
			Ada.Text_IO.New_Line;
			Ada.Text_IO.Put_Line (":file: `" & Contents.File_Name.Constant_Reference.Element.all
				& " <" & Prefix & Contents.Relative_Name.Constant_Reference.Element.all & ">`_");
			if not Contents.Renamed.Is_Null then
				Ada.Text_IO.Put_Line (":renames: `" & Contents.Renamed.Constant_Reference.Element.all & "`");
			end if;
			if not Contents.Instantiation.Is_Null then
				Ada.Text_IO.Put_Line (":instantiation: `" & Contents.Instantiation.Constant_Reference.Element.all & "`");
			end if;
			if not Contents.Reference.Is_Null then
				Ada.Text_IO.Put_Line (":reference: `" & Contents.Reference.Constant_Reference.Element.all & "`");
			end if;
			Ada.Text_IO.New_Line;
			if not Contents.Document.Is_Null then
				Ada.Text_IO.Put_Line (Contents.Document.Constant_Reference.Element.all);
			end if;
		end Output_Unit;
	begin
		Ada.Text_IO.Put_Line ("Standard packages");
		Ada.Text_IO.Put_Line ("*****************");
		Ada.Text_IO.New_Line;
		declare
			I : Doc_Maps.Cursor := Extendeds.First;
		begin
			while Doc_Maps.Has_Element (I) loop
				if Extendeds.Constant_Reference (I).Element.Kind = Standard_Unit then
					Output_Unit (I);
				end if;
				Doc_Maps.Next (I);
			end loop;
		end;
		Ada.Text_IO.Put_Line ("Additional packages");
		Ada.Text_IO.Put_Line ("*******************");
		Ada.Text_IO.New_Line;
		declare
			I : Doc_Maps.Cursor := Extendeds.First;
		begin
			while Doc_Maps.Has_Element (I) loop
				if Extendeds.Constant_Reference (I).Element.Kind = Extended_Unit then
					Output_Unit (I);
				end if;
				Doc_Maps.Next (I);
			end loop;
		end;
	end;
end ext_doc;
