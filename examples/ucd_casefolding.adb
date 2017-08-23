-- convert UCD/CaseFolding.txt
-- bin/ucd_casefolding $UCD/CaseFolding.txt > ../source/strings/a-uccafo.ads
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
procedure ucd_casefolding is
	Hexadecimal_Digit_Set : constant Character_Set := To_Set ("0123456789ABCDEF");
	function Value (S : String) return Wide_Wide_Character is
		Img : constant String := "Hex_" & (1 .. 8 - S'Length => '0') & S;
	begin
		return Wide_Wide_Character'Value (Img);
	end Value;
	package CF_Maps is
		new Ada.Containers.Ordered_Maps (
			Wide_Wide_Character,
			Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);
	use CF_Maps;
	function Compressible (I : CF_Maps.Cursor) return Boolean is
	begin
		return Length (Element (I)) = 1
			and then
				Wide_Wide_Character'Pos (Element (Element (I), 1)) - Wide_Wide_Character'Pos (Key (I))
				in -128 .. 127;
	end Compressible;
	type Kind_Type is (C, F, S, T); -- common, full, simple, special
	Table : array (Kind_Type) of aliased CF_Maps.Map;
	type Bit is (In_16, In_32);
	function Get_Bit (C : Wide_Wide_Character) return Bit is
	begin
		if C > Wide_Wide_Character'Val (16#FFFF#) then
			return In_32;
		else
			return In_16;
		end if;
	end Get_Bit;
	subtype Len is Integer range 1 .. 3;
	Num : array (Kind_Type, Bit, Len, Boolean) of Natural;
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
				Source : Wide_Wide_Character;
				Dest : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
				Kind : Kind_Type;
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
					Source := Value (Line (Token_First .. Token_Last));
					P := Token_Last + 1;
					if Line (P) /= ';' then
						raise Data_Error with Line & " -- B";
					end if;
					P := P + 1; -- skip ';'
					N := Index_Non_Blank (Line (P .. Line'Last));
					if N = 0 then
						raise Data_Error with Line & " -- C";
					end if;
					P := N;
					case Line (P) is
						when 'C' => Kind := C;
						when 'F' => Kind := F;
						when 'S' => Kind := S;
						when 'T' => Kind := T;
						when others => raise Data_Error with Line & " -- D";
					end case;
					P := P + 1; -- skip kind
					if Line (P) /= ';' then
						raise Data_Error with Line & " -- E";
					end if;
					P := P + 1; -- skip ';'
					Dest := Null_Unbounded_Wide_Wide_String;
					loop
						N := Index_Non_Blank (Line (P .. Line'Last));
						if N = 0 then
							raise Data_Error with Line & " -- F";
						end if;
						P := N;
						exit when Line (P) = ';';
						Find_Token (
							Line (P .. Line'Last),
							Hexadecimal_Digit_Set,
							Inside,
							Token_First,
							Token_Last);
						if Token_First /= P then
							raise Data_Error with Line & " -- G";
						end if;
						Append (Dest, Value (Line (Token_First .. Token_Last)));
						P := Token_Last + 1;
					end loop;
					Insert (Table (Kind), Source, Dest);
				end if;
			end;
		end loop;
		Close (File);
	end;
	for Kind in Kind_Type loop
		for B in Bit loop
			for L in Len loop
				for Compressed in Boolean loop
					Num (Kind, B, L, Compressed) := 0;
				end loop;
			end loop;
		end loop;
		declare
			I : CF_Maps.Cursor := First (Table (Kind));
		begin
			while Has_Element (I) loop
				declare
					B : Bit := Get_Bit (Key (I));
					L : Len;
				begin
					if Compressible (I) then
						declare
							K : Wide_Wide_Character := Key (I);
							E : Wide_Wide_Character := Element (Element (I), 1);
							N : CF_Maps.Cursor := Next (I);
							RLE : Positive := 1;
							Compressed : Boolean;
						begin
							while Has_Element (N)
								and then RLE < 255
								and then Compressible (N)
								and then Key (N) = Wide_Wide_Character'Succ (K)
								and then Element (Element (N), 1) = Wide_Wide_Character'Succ (E)
							loop
								K := Key (N);
								E := Element (Element (N), 1);
								N := Next (N);
								RLE := RLE + 1;
							end loop;
							I := N;
							Compressed := RLE > 1;
							Num (Kind, B, 1, Compressed) := Num (Kind, B, 1, Compressed) + 1;
						end;
					else
						L := Length (Element (I));
						Num (Kind, B, L, False) := Num (Kind, B, L, False) + 1;
						I := Next (I);
					end if;
				end;
			end loop;
		end;
	end loop;
	Put_Line ("pragma License (Unrestricted);");
	Put_Line ("--  implementation unit, translated from CaseFolding.txt");
	Put_Line ("package Ada.UCD.Case_Folding is");
	Put_Line ("   pragma Pure;");
	New_Line;
	for Kind in Kind_Type loop
		Put ("   ");
		Put (Kind_Type'Image (Kind));
		Put ("_Total : constant := ");
		Put (Integer (Length (Table (Kind))), Width => 1);
		Put (";");
		New_Line;
	end loop;
	New_Line;
	Put_Line ("   type Run_Length_8 is mod 2 ** 8;");
	New_Line;
	Put_Line ("   type Compressed_Item_Type is record");
	Put_Line ("      Start : UCS_2;");
	Put_Line ("      Length : Run_Length_8;");
	Put_Line ("      Diff : Difference_8;");
	Put_Line ("   end record;");
	Put_Line ("   pragma Suppress_Initialization (Compressed_Item_Type);");
	Put_Line ("   for Compressed_Item_Type'Size use 32; -- 16 + 8 + 8");
	Put_Line ("   for Compressed_Item_Type use record");
	Put_Line ("      Start at 0 range 0 .. 15;");
	Put_Line ("      Length at 0 range 16 .. 23;");
	Put_Line ("      Diff at 0 range 24 .. 31;");
	Put_Line ("   end record;");
	New_Line;
	Put_Line ("   type Compressed_Type is array (Positive range <>) of Compressed_Item_Type;");
	Put_Line ("   pragma Suppress_Initialization (Compressed_Type);");
	Put_Line ("   for Compressed_Type'Component_Size use 32;");
	New_Line;
	Put ("   subtype C_Table_XXXXx1_Type is Map_16x1_Type (1 .. ");
	Put (Num (C, In_16, 1, False), Width => 1);
	Put (");");
	New_Line;
	Put ("   subtype C_Table_XXXXx1_Compressed_Type is Compressed_Type (1 .. ");
	Put (Num (C, In_16, 1, True), Width => 1);
	Put (");");
	New_Line;
	Put ("   subtype C_Table_1XXXXx1_Compressed_Type is Compressed_Type (1 .. ");
	Put (Num (C, In_32, 1, True), Width => 1);
	Put (");");
	New_Line;
	if Num (C, In_16, 2, False) /= 0
		or else Num (C, In_16, 3, False) /= 0
		or else Num (C, In_32, 1, False) /= 0
		or else Num (C, In_32, 2, False) /= 0
		or else Num (C, In_32, 3, False) /= 0
	then
		raise Data_Error with "num of C";
	end if;
	Put ("   subtype F_Table_XXXXx2_Type is Map_16x2_Type (1 .. ");
	Put (Num (F, In_16, 2, False), Width => 1);
	Put (");");
	New_Line;
	Put ("   subtype F_Table_XXXXx3_Type is Map_16x3_Type (1 .. ");
	Put (Num (F, In_16, 3, False), Width => 1);
	Put (");");
	New_Line;
	if Num (F, In_16, 1, False) /= 0
		or else Num (F, In_16, 1, True) /= 0
		or else Num (F, In_32, 1, False) /= 0
		or else Num (F, In_32, 1, True) /= 0
		or else Num (F, In_32, 2, False) /= 0
		or else Num (F, In_32, 3, False) /= 0
	then
		raise Data_Error with "num of S";
	end if;
	Put ("   subtype S_Table_XXXXx1_Type is Map_16x1_Type (1 .. ");
	Put (Num (S, In_16, 1, False), Width => 1);
	Put (");");
	New_Line;
	Put ("   subtype S_Table_XXXXx1_Compressed_Type is Compressed_Type (1 .. ");
	Put (Num (S, In_16, 1, True), Width => 1);
	Put (");");
	New_Line;
	if Num (S, In_16, 2, False) /= 0
		or else Num (S, In_16, 3, False) /= 0
		or else Num (S, In_32, 1, False) /= 0
		or else Num (S, In_32, 1, True) /= 0
		or else Num (S, In_32, 2, False) /= 0
		or else Num (S, In_32, 3, False) /= 0
	then
		raise Data_Error with "num of S";
	end if;
	Put ("   subtype T_Table_XXXXx1_Type is Map_16x1_Type (1 .. ");
	Put (Num (T, In_16, 1, False), Width => 1);
	Put (");");
	New_Line;
	if Num (T, In_16, 1, True) /= 0
		or else Num (T, In_16, 2, False) /= 0
		or else Num (T, In_16, 3, False) /= 0
		or else Num (T, In_32, 1, False) /= 0
		or else Num (T, In_32, 1, True) /= 0
		or else Num (T, In_32, 2, False) /= 0
		or else Num (T, In_32, 3, False) /= 0
	then
		raise Data_Error with "num of S";
	end if;
	New_Line;
	for Kind in Kind_Type loop
		for B in Bit loop
			for L in Len loop
				for Compressed in Boolean loop
					if Num (Kind, B, L, Compressed) /= 0 then
						Put ("   ");
						Put (Kind_Type'Image (Kind));
						Put ("_Table_");
						if B = In_32 then
							Put ("1");
						end if;
						Put ("XXXXx");
						Put (L, Width => 1);
						if Compressed then
							Put ("_Compressed");
						end if;
						Put (" : constant ");
						Put (Kind_Type'Image (Kind));
						Put ("_Table_");
						if B = In_32 then
							Put ("1");
						end if;
						Put ("XXXXx");
						Put (L, Width => 1);
						if Compressed then
							Put ("_Compressed");
						end if;
						Put ("_Type := (");
						New_Line;
						declare
							Offset : Integer := 0;
							I : CF_Maps.Cursor := First (Table (Kind));
							Second : Boolean := False;
						begin
							if B = In_32 then
								Offset := 16#10000#;
							end if;
							while Has_Element (I) loop
								declare
									Item_B : Bit := Get_Bit (Key (I));
									Item_L : Len := Length (Element (I));
									Item_RLE : Positive := 1;
									N : CF_Maps.Cursor := Next (I);
								begin
									if Compressible (I) then
										declare
											K : Wide_Wide_Character := Key (I);
											E : Wide_Wide_Character := Element (Element (I), 1);
										begin
											while Has_Element (N)
												and then Item_RLE < 255
												and then Compressible (N)
												and then Key (N) = Wide_Wide_Character'Succ (K)
												and then Element (Element (N), 1) = Wide_Wide_Character'Succ (E)
											loop
												K := Key (N);
												E := Element (Element (N), 1);
												N := Next (N);
												Item_RLE := Item_RLE + 1;
											end loop;
										end;
									end if;
									if Item_B = B
										and then Item_L = L
										and then (Item_RLE > 1) = Compressed
									then
										if Second then
											Put (",");
											New_Line;
										end if;
										Put ("      ");
										if Num (Kind, B, L, Compressed) = 1 then
											Put ("1 => ");
										end if;
										Put ("(");
										Put (
											Integer'(Wide_Wide_Character'Pos (Key (I))) - Offset,
											Base => 16,
											Width => 8, -- 16#XXXX#
											Padding => '0');
										Put (", ");
										if Compressed then
											Put (Item_RLE, Width => 1);
											Put (", ");
											Put (
												Wide_Wide_Character'Pos (Element (Element (I), 1))
												- Wide_Wide_Character'Pos (Key (I)),
												Width => 1);
										else
											if L > 1 then
												Put ("(");
											end if;
											for J in 1 .. Item_L loop
												if J > 1 then
													Put (", ");
												end if;
												Put (
													Integer'(Wide_Wide_Character'Pos (Element (Element (I), J))) - Offset,
													Base => 16,
													Width => 8, -- 16#XXXX#
													Padding => '0');
											end loop;
											if L > 1 then
												Put (")");
											end if;
										end if;
										Put (")");
										Second := True;
									end if;
									I := N;
								end;
							end loop;
							Put (");");
							New_Line;
						end;
						New_Line;
					end if;
				end loop;
			end loop;
		end loop;
	end loop;
	Put_Line ("end Ada.UCD.Case_Folding;");
end ucd_casefolding;
