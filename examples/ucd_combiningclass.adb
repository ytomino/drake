-- convert UCD/extracted/DerivedCombiningClass.txt
-- bin/ucd_combiningclass $UCD/extracted/DerivedCombiningClass.txt > ../source/strings/a-uccocl.ads
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Text_IO; use Ada.Text_IO;
procedure ucd_combiningclass is
	function Value (S : String) return Wide_Wide_Character is
		Img : constant String := "Hex_" & (1 .. 8 - S'Length => '0') & S;
	begin
		return Wide_Wide_Character'Value (Img);
	end Value;
	type Combining_Class is mod 2 ** 8;
	package CC_IO is new Ada.Text_IO.Modular_IO (Combining_Class);
	use CC_IO;
	package CP2CC_Maps is
		new Ada.Containers.Ordered_Maps (Wide_Wide_Character, Combining_Class);
	use CP2CC_Maps;
	Table : aliased CP2CC_Maps.Map;
	In_16_Num, In_32_Num : Natural;
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
				CC : Combining_Class;
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
						Decimal_Digit_Set,
						Inside,
						Token_First,
						Token_Last);
					if Token_First /= P then
						raise Data_Error with Line & " -- E";
					end if;
					CC := Combining_Class'Value (Line (Token_First .. Token_Last));
					if CC /= 0 then
						for I in First .. Last loop
							Insert (Table, I, CC);
						end loop;
					end if;
				end if;
			end;
		end loop;
		Close (File);
	end;
	declare
		I : CP2CC_Maps.Cursor := First (Table);
	begin
		In_16_Num := 0;
		In_32_Num := 0;
		while Has_Element (I) loop
			declare
				CC : Combining_Class := Element (I);
				L : CP2CC_Maps.Cursor := I;
				N : CP2CC_Maps.Cursor := Next (I);
			begin
				while Has_Element (N)
					and then Key (N) = Wide_Wide_Character'Succ (Key (L))
					and then Element (N) = CC
				loop
					L := N;
					N := Next (N);
				end loop;
				if Key (L) > Wide_Wide_Character'Val (16#FFFF#) then
					In_32_Num := In_32_Num + 1;
				else
					In_16_Num := In_16_Num + 1;
				end if;
				I := N;
			end;
		end loop;
	end;
	Put_Line ("pragma License (Unrestricted);");
	Put_Line ("--  translated unit from DerivedCombiningClass.txt");
	Put_Line ("package Ada.UCD.Combining_Class is");
	Put_Line ("   pragma Pure;");
	New_Line;
	Put_Line ("   type Run_Length_8 is mod 2 ** 8;");
	New_Line;
	Put_Line ("   type Table_16_Item_Type is record");
	Put_Line ("      Start : UCS_2;");
	Put_Line ("      Length : Run_Length_8;");
	Put_Line ("      Combining_Class : Combining_Class_Type;");
	Put_Line ("   end record;");
	Put_Line ("   pragma Suppress_Initialization (Table_16_Item_Type);");
	Put_Line ("   pragma Pack (Table_16_Item_Type);");
	Put_Line ("   pragma Compile_Time_Error (Table_16_Item_Type'Size /= 32, ""packed?"");");
	New_Line;
	Put_Line ("   type Table_16_Type is array (Positive range <>) of Table_16_Item_Type;");
	Put_Line ("   pragma Suppress_Initialization (Table_16_Type);");
	Put_Line ("   pragma Pack (Table_16_Type);");
	Put_Line ("   pragma Compile_Time_Error (Table_16_Type'Component_Size /= 32, ""packed?"");");
	New_Line;
	Put ("   subtype Table_XXXX_Type is Table_16_Type (1 .. ");
	Put (In_16_Num, Width => 1);
	Put (");");
	New_Line;
	New_Line;
	Put ("   subtype Table_1XXXX_Type is Table_16_Type (");
	Put (In_16_Num + 1, Width => 1);
	Put (" .. ");
	Put (In_16_Num + In_32_Num, Width => 1);
	Put (");");
	New_Line;
	New_Line;
	Put_Line ("   Table_XXXX : constant Table_XXXX_Type := (");
	declare
		type State_Type is (In_16, In_32);
		State : State_Type := In_16;
		Offset : Integer := 0;
		I : CP2CC_Maps.Cursor := First (Table);
	begin
		while Has_Element (I) loop
			declare
				CC : Combining_Class := Element (I);
				L : CP2CC_Maps.Cursor := I;
				N : CP2CC_Maps.Cursor := Next (I);
			begin
				while Has_Element (N)
					and then Key (N) = Wide_Wide_Character'Succ (Key (L))
					and then Element (N) = CC
				loop
					L := N;
					N := Next (N);
				end loop;
				Put ("      (");
				Put (
					Integer'(Wide_Wide_Character'Pos (Key (I))) - Offset,
					Base => 16,
					Width => 8, -- 16#XXXX#
					Padding => '0');
				Put (", ");
				Put (
					Integer'(Wide_Wide_Character'Pos (Key (L)))
					- Integer'(Wide_Wide_Character'Pos (Key (I)))
					+ 1,
					Width => 1);
				Put (", ");
				Put (CC, Width => 1);
				Put (")");
				if Has_Element (N) then
					if State = In_16
						and then Key (N) > Wide_Wide_Character'Val (16#FFFF#)
					then
						State := In_32;
						Offset := 16#10000#;
						Put (");");
						New_Line;
						New_Line;
						Put_Line ("   Table_1XXXX : constant Table_1XXXX_Type := (");
					else
						Put (",");
						New_Line;
					end if;
				else
					Put (");");
					New_Line;
				end if;
				I := N;
			end;
		end loop;
	end;
	New_Line;
	Put_Line ("end Ada.UCD.Combining_Class;");
end ucd_combiningclass;
