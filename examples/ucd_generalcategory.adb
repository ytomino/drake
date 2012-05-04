-- convert UCD/extracted/DerivedGeneralCategory.txt
-- build/ucd_generalcategory UCD/extracted/DerivedGeneralCategory.txt > ../source/strings/a-ucgeca.ads
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
procedure ucd_generalcategory is
	function Value (S : String) return Wide_Wide_Character is
		Img : constant String := "Hex_" & (1 .. 8 - S'Length => '0') & S;
	begin
		return Wide_Wide_Character'Value (Img);
	end Value;
	type Bit is (In_16, In_17, In_32);
	function Get_Bit (C : Wide_Wide_Character) return Bit is
	begin
		if C > Wide_Wide_Character'Val (16#1FFFF#) then
			return In_32;
		elsif C > Wide_Wide_Character'Val (16#FFFF#) then
			return In_17;
		else
			return In_16;
		end if;
	end Get_Bit;
	type Range_Item is record
		First : Wide_Wide_Character;
		Last : Wide_Wide_Character;
	end record;
	package Range_Lists is new Ada.Containers.Doubly_Linked_Lists (Range_Item);
	use Range_Lists;
	type Nums_Type is array (Bit, Boolean) of Natural;
	type Category_Item is record
		Name : Ada.Strings.Unbounded.Unbounded_String;
		Code_Points : aliased Range_Lists.List;
		Total_CP_Num : Natural;
		Total_Set_Num : Natural;
		Nums : Nums_Type;
	end record;
	package Category_Lists is new Ada.Containers.Doubly_Linked_Lists (Category_Item);
	use Category_Lists;
	function Find (Table : Category_Lists.List; Category : String) return Category_Lists.Cursor is
		I : Category_Lists.Cursor := First (Table);
	begin
		while Has_Element (I) loop
			if Table.Constant_Reference (I).Element.Name = Category then
				return I;
			end if;
			I := Next (I);
		end loop;
		return Category_Lists.No_Element;
	end Find;
	Table : aliased Category_Lists.List;
begin
	declare
		File : Ada.Text_IO.File_Type;
	begin
		Open (File, In_File, Argument (1));
		while not End_Of_File (File) loop
			declare
				Line : constant String := Get_Line (File);
				P : Positive := Line'First;
				N : Natural;
				Token_First : Positive;
				Token_Last : Natural;
				First, Last : Wide_Wide_Character;
				Category : Ada.Strings.Unbounded.Unbounded_String;
				Category_Pos : Category_Lists.Cursor;
			begin
				if Line'Length = 0 or else Line (P) = '#' then
					null; -- comment
				else
					Find_Token (
						Line (P .. Line'Last),
						Hexadecimal_Digit_Set,
						Inside,
						Token_First,
						Token_Last);
					if Token_First /= P then
						raise Data_Error with Line & " -- A";
					end if;
					First := Value (Line (Token_First .. Token_Last));
					P := Token_Last + 1;
					if Line (P) = '.' and then Line (P + 1) = '.' then
						P := P + 2;
						Find_Token (
							Line (P .. Line'Last),
							Hexadecimal_Digit_Set,
							Inside,
							Token_First,
							Token_Last);
						if Token_First /= P then
							raise Data_Error with Line & " -- B";
						end if;
						Last := Value (Line (Token_First .. Token_Last));
						P := Token_Last + 1;
					else
						Last := First;
					end if;
					N := Index_Non_Blank (Line (P .. Line'Last));
					if N = 0 or else Line (N) /= ';' then
						raise Data_Error with Line & " -- C";
					end if;
					P := N + 1; -- skip ';'
					N := Index_Non_Blank (Line (P .. Line'Last));
					if N = 0 then
						raise Data_Error with Line & " -- D";
					end if;
					P := N;
					Find_Token (
						Line (P .. Line'Last),
						Letter_Set,
						Inside,
						Token_First,
						Token_Last);
					if Token_First /= P then
						raise Data_Error with Line & " -- E";
					end if;
					Category := To_Unbounded_String (Line (Token_First .. Token_Last));
					Category_Pos := Find (Table, To_String (Category));
					if not Has_Element (Category_Pos) then
						Insert (
							Table,
							Before => Category_Lists.No_Element,
							Position => Category_Pos);
						Table.Reference (Category_Pos).Element.Name := Category;
					end if;
					Append (
						Table.Reference (Category_Pos).Element.Code_Points,
						(First => First, Last => Last));
				end if;
			end;
		end loop;
		Close (File);
	end;
	declare
		I : Category_Lists.Cursor := First (Table);
	begin
		while Has_Element (I) loop
			declare
				Cat : Category_Item renames Table.Reference (I).Element.all;
			begin
				Cat.Total_CP_Num := 0;
				Cat.Total_Set_Num := 0;
				for J in Bit loop
					for K in Boolean loop
						Cat.Nums (J, K) := 0;
					end loop;
				end loop;
				declare
					J : Range_Lists.Cursor := First (Cat.Code_Points);
				begin
					while Has_Element (J) loop
						declare
							R : Range_Item renames Cat.Code_Points.Constant_Reference (J).Element.all;
							B : Bit := Get_Bit (R.Last);
							S : Boolean := R.First /= R.Last;
						begin
							Cat.Nums (B, S) := Cat.Nums (B, S) + 1;
							Cat.Total_CP_Num := Cat.Total_CP_Num + (
								Wide_Wide_Character'Pos (R.Last)
								- Wide_Wide_Character'Pos (R.First)
								+ 1);
							Cat.Total_Set_Num := Cat.Total_Set_Num + 1;
						end;
						J := Next (J);
					end loop;
				end;
			end;
			I := Next (I);
		end loop;
	end;
	Put_Line ("pragma License (Unrestricted);");
	Put_Line ("--  translated unit from DerivedGeneralCategory.txt");
	Put_Line ("package Ada.UCD.General_Category is");
	Put_Line ("   pragma Pure;");
	New_Line;
	declare
		I : Category_Lists.Cursor := First (Table);
	begin
		while Has_Element (I) loop
			declare
				Cat : Category_Item renames Table.Reference (I).Element.all;
			begin
				Put ("   ");
				Put (To_String (Cat.Name));
				Put ("_Total : constant := ");
				Put (Cat.Total_CP_Num, Width => 1);
				Put (";");
				New_Line;
				Put ("   ");
				Put (To_String (Cat.Name));
				Put ("_Range_Length : constant := ");
				Put (Cat.Total_Set_Num, Width => 1);
				Put (";");
				New_Line;
			end;
			I := Next (I);
		end loop;
	end;
	New_Line;
	Put_Line ("   Dummy_XXXXx1 : constant UCS_2_Array (1 .. 0) := (others => <>);");
	Put_Line ("   Dummy_XXXXx2 : constant Set_16_Type (1 .. 0) := (others => <>);");
	Put_Line ("   Dummy_1XXXXx1 : UCS_2_Array renames Dummy_XXXXx1;");
	Put_Line ("   Dummy_1XXXXx2 : Set_16_Type renames Dummy_XXXXx2;");
	Put_Line ("   Dummy_XXXXXXXXx1 : constant UCS_4_Array (1 .. 0) := (others => <>);");
	Put_Line ("   Dummy_XXXXXXXXx2 : constant Set_32_Type (1 .. 0) := (others => <>);");
	New_Line;
	declare
		I : Category_Lists.Cursor := First (Table);
	begin
		while Has_Element (I) loop
			declare
				Cat : Category_Item renames Table.Reference (I).Element.all;
			begin
				for B in Bit loop
					for S in Boolean loop
						Put ("   ");
						Put (To_String (Cat.Name));
						Put ("_Table_");
						case B is
							when In_32 => Put ("XXXXXXXX");
							when In_17 => Put ("1XXXX");
							when In_16 => Put ("XXXX");
						end case;
						if S then
							Put ("x2");
						else
							Put ("x1");
						end if;
						Put (" : ");
						if Cat.Nums (B, S) > 0 then
							Put ("constant ");
						end if;
						if S then
							case B is
								when In_32 => Put ("Set_32_Type");
								when In_16 | In_17 => Put ("Set_16_Type");
							end case;
						else
							case B is
								when In_32 => Put ("UCS_4_Array");
								when In_16 | In_17 => Put ("UCS_2_Array");
							end case;
						end if;
						if Cat.Nums (B, S) = 0 then
							Put (" renames Dummy_");
							case B is
								when In_32 => Put ("XXXXXXXX");
								when In_17 => Put ("1XXXX");
								when In_16 => Put ("XXXX");
							end case;
							if S then
								Put ("x2");
							else
								Put ("x1");
							end if;
							Put (";");
							New_Line;
						else
							Put (" (1 .. ");
							Put (Cat.Nums (B, S), Width => 1);
							Put (") := (");
							New_Line;
							declare
								J : Range_Lists.Cursor := First (Cat.Code_Points);
								Second : Boolean := False;
							begin
								while Has_Element (J) loop
									declare
										R : Range_Item renames Cat.Code_Points.Constant_Reference (J).Element.all;
										Item_B : Bit := Get_Bit (R.Last);
										Item_S : Boolean := R.First /= R.Last;
										Offset : Integer := 0;
									begin
										if Item_B = B and Item_S = S then
											if B = In_17 then
												Offset := 16#10000#;
											end if;
											if Second then
												Put (",");
												New_Line;
											end if;
											Put ("      ");
											if Cat.Nums (B, S) = 1 then
												Put ("1 => ");
											end if;
											if S then
												Put ("(");
												Put (
													Integer'(Wide_Wide_Character'Pos (R.First)) - Offset,
													Base => 16,
													Width => 8, -- 16#XXXX#
													Padding => '0');
												Put (", ");
												Put (
													Integer'(Wide_Wide_Character'Pos (R.Last)) - Offset,
													Base => 16,
													Width => 8, -- 16#XXXX#
													Padding => '0');
												Put (")");
											else
												Put (
													Integer'(Wide_Wide_Character'Pos (R.First)) - Offset,
													Base => 16,
													Width => 8, -- 16#XXXX#
													Padding => '0');
											end if;
											Second := True;
										end if;
									end;
									J := Next (J);
								end loop;
							end;
							Put (");");
							New_Line;
						end if;
						New_Line;
					end loop;
				end loop;
			end;
			I := Next (I);
		end loop;
	end;
	Put_Line ("end Ada.UCD.General_Category;");
end ucd_generalcategory;
