-- convert UCD/extracted/DerivedEastAsianWidth.txt
-- bin/ucd_eastasianwidth $UCD/extracted/DerivedEastAsianWidth.txt > ../source/strings/a-ueaswi.ads
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Bounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Text_IO; use Ada.Text_IO;
procedure ucd_eastasianwidth is
	function Value (S : String) return Wide_Wide_Character is
		Img : constant String := "Hex_" & (1 .. 8 - S'Length => '0') & S;
	begin
		return Wide_Wide_Character'Value (Img);
	end Value;
	package EAW_Property_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (2);
	use type EAW_Property_Names.Bounded_String;
	package CP2EAW_Maps is
		new Ada.Containers.Ordered_Maps (Wide_Wide_Character, EAW_Property_Names.Bounded_String);
	use CP2EAW_Maps;
	Table : aliased CP2EAW_Maps.Map;
	type Bit is (In_XXXX, In_1XXXX, In_XXXXXXXX);
	Num : array (Bit) of Natural;
begin
	declare
		File : Ada.Text_IO.File_Type;
	begin
		Open (File, In_File, Argument (1));
		while not End_Of_File (File) loop
			declare
				Line : constant String := Get_Line (File);
				P : Positive := Line'First;
				Next : Natural;
				Token_First : Positive;
				Token_Last : Natural;
				First, Last : Wide_Wide_Character;
				EAW : EAW_Property_Names.Bounded_String;
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
					Next := Index_Non_Blank (Line (P .. Line'Last));
					if Next = 0 or else Line (Next) /= ';' then
						raise Data_Error with Line & " -- C";
					end if;
					P := Next + 1; -- skip ';'
					Next := Index_Non_Blank (Line (P .. Line'Last));
					if Next = 0 then
						raise Data_Error with Line & " -- D";
					end if;
					P := Next;
					Find_Token (
						Line (P .. Line'Last),
						Letter_Set,
						Inside,
						Token_First,
						Token_Last);
					if Token_First /= P then
						raise Data_Error with Line & " -- E";
					end if;
					EAW := +Line (Token_First .. Token_Last);
					if EAW /= "N" then
						for I in First .. Last loop
							Insert (Table, I, EAW);
						end loop;
					end if;
				end if;
			end;
		end loop;
		Close (File);
	end;
	declare
		I : CP2EAW_Maps.Cursor := First (Table);
	begin
		for I in Num'Range loop
			Num (I) := 0;
		end loop;
		while Has_Element (I) loop
			declare
				EAW : EAW_Property_Names.Bounded_String := Element (I);
				L : CP2EAW_Maps.Cursor := I;
				N : CP2EAW_Maps.Cursor := Next (I);
			begin
				while Has_Element (N)
					and then Key (N) = Wide_Wide_Character'Succ (Key (L))
					and then Element (N) = EAW
				loop
					L := N;
					N := Next (N);
				end loop;
				if Key (L) <= Wide_Wide_Character'Val (16#FFFF#) then
					Num (In_XXXX) := Num (In_XXXX)
						+ Integer'(
							Wide_Wide_Character'Pos (Key (L))
							- Wide_Wide_Character'Pos (Key (I))
							+ 1
							+ (2 ** 13 - 1))
						/ 2 ** 13;
				elsif Key (L) <= Wide_Wide_Character'Val (16#1FFFF#) then
					Num (In_1XXXX) := Num (In_1XXXX) + 1;
				else
					Num (In_XXXXXXXX) := Num (In_XXXXXXXX) + 1;
				end if;
				I := N;
			end;
		end loop;
	end;
	Put_Line ("pragma License (Unrestricted);");
	Put_Line ("--  implementation unit, translated from DerivedEastAsianWidth.txt");
	Put_Line ("package Ada.UCD.East_Asian_Width is");
	Put_Line ("   pragma Pure;");
	New_Line;
	Put_Line ("   type Run_Length_13 is mod 2 ** 13;");
	Put_Line ("   type Run_Length_29 is mod 2 ** 29;");
	New_Line;
	Put_Line ("   type Table_16_Item_Type is record");
	Put_Line ("      Start : UCS_2;");
	Put_Line ("      Length : Run_Length_13;");
	Put_Line ("      Width : East_Asian_Width_Type;");
	Put_Line ("   end record;");
	Put_Line ("   pragma Suppress_Initialization (Table_16_Item_Type);");
	Put_Line ("   for Table_16_Item_Type'Size use 32; -- 16 + 13 + 3");
	Put_Line ("   for Table_16_Item_Type use record");
	Put_Line ("      Start at 0 range 0 .. 15;");
	Put_Line ("      Length at 0 range 16 .. 28;");
	Put_Line ("      Width at 0 range 29 .. 31;");
	Put_Line ("   end record;");
	New_Line;
	Put_Line ("   type Table_16_Type is array (Positive range <>) of Table_16_Item_Type;");
	Put_Line ("   pragma Suppress_Initialization (Table_16_Type);");
	Put_Line ("   for Table_16_Type'Component_Size use 32;");
	New_Line;
	Put_Line ("   type Table_32_Item_Type is record");
	Put_Line ("      Start : UCS_4;");
	Put_Line ("      Length : Run_Length_29;");
	Put_Line ("      Width : East_Asian_Width_Type;");
	Put_Line ("   end record;");
	Put_Line ("   pragma Suppress_Initialization (Table_32_Item_Type);");
	Put_Line ("   for Table_32_Item_Type'Size use 64; -- 32 + 29 + 3");
	Put_Line ("   for Table_32_Item_Type use record");
	Put_Line ("      Start at 0 range 0 .. 31;");
	Put_Line ("      Length at 0 range 32 .. 60;");
	Put_Line ("      Width at 0 range 61 .. 63;");
	Put_Line ("   end record;");
	New_Line;
	Put_Line ("   type Table_32_Type is array (Positive range <>) of Table_32_Item_Type;");
	Put_Line ("   pragma Suppress_Initialization (Table_32_Type);");
	Put_Line ("   for Table_32_Type'Component_Size use 64;");
	New_Line;
	Put ("   subtype Table_XXXX_Type is Table_16_Type (1 .. ");
	Put (Num (In_XXXX), Width => 1);
	Put (");");
	New_Line;
	New_Line;
	Put ("   subtype Table_1XXXX_Type is Table_16_Type (");
	Put (Num (In_XXXX) + 1, Width => 1);
	Put (" .. ");
	Put (Num (In_XXXX) + Num (In_1XXXX), Width => 1);
	Put (");");
	New_Line;
	New_Line;
	Put ("   subtype Table_XXXXXXXX_Type is Table_32_Type (");
	Put (Num (In_XXXX) + Num (In_1XXXX) + 1, Width => 1);
	Put (" .. ");
	Put (Num (In_XXXX) + Num (In_1XXXX) + Num (In_XXXXXXXX), Width => 1);
	Put (");");
	New_Line;
	New_Line;
	Put_Line ("   Table_XXXX : constant Table_XXXX_Type := (");
	declare
		State : Bit := In_XXXX;
		Offset : Integer := 0;
		I : CP2EAW_Maps.Cursor := First (Table);
	begin
		while Has_Element (I) loop
			declare
				EAW : EAW_Property_Names.Bounded_String renames Table.Constant_Reference (I).Element.all;
				L : CP2EAW_Maps.Cursor := I;
				N : CP2EAW_Maps.Cursor := Next (I);
			begin
				while Has_Element (N)
					and then Key (N) = Wide_Wide_Character'Succ (Key (L))
					and then Element (N) = EAW
				loop
					L := N;
					N := Next (N);
					exit when State = In_XXXX
						and then
							Wide_Wide_Character'Pos (Key (L))
							- Wide_Wide_Character'Pos (Key (I))
							+ 1 = 2 ** 13 - 1;
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
				Put (EAW_Property_Names.Bounded_Strings.Constant_Reference (EAW).Element.all);
				Put (")");
				if Has_Element (N) then
					if State = In_XXXX
						and then Key (N) > Wide_Wide_Character'Val (16#FFFF#)
					then
						State := In_1XXXX;
						Offset := 16#10000#;
						Put (");");
						New_Line;
						New_Line;
						Put_Line ("   Table_1XXXX : constant Table_1XXXX_Type := (");
					elsif State = In_1XXXX
						and then Key (N) > Wide_Wide_Character'Val (16#1FFFF#)
					then
						State := In_XXXXXXXX;
						Offset := 0;
						Put (");");
						New_Line;
						New_Line;
						Put_Line ("   Table_XXXXXXXX : constant Table_XXXXXXXX_Type := (");
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
	Put_Line ("end Ada.UCD.East_Asian_Width;");
end ucd_eastasianwidth;
