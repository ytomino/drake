-- convert UCD/UnicodeData.txt (13, 14)
-- bin/ucd_simplecasemapping $UCD/UnicodeData.txt > ../source/strings/a-uscama.ads
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
procedure ucd_simplecasemapping is
	function Value (S : String) return Wide_Wide_Character is
		Img : constant String := "Hex_" & (1 .. 8 - S'Length => '0') & S;
	begin
		return Wide_Wide_Character'Value (Img);
	end Value;
	package WWC_Maps is
		new Ada.Containers.Ordered_Maps (
			Wide_Wide_Character,
			Wide_Wide_Character);
	use WWC_Maps;
	function Compressible (I : WWC_Maps.Cursor) return Boolean is
	begin
		return Wide_Wide_Character'Pos (Element (I)) - Wide_Wide_Character'Pos (Key (I))
			in -128 .. 127;
	end Compressible;
	Upper_Table, Lower_Table, Shared_Table : WWC_Maps.Map;
	Upper_Num, Lower_Num : Natural;
	type Bit is (In_16, In_32);
	function Get_Bit (C : Wide_Wide_Character) return Bit is
	begin
		if C > Wide_Wide_Character'Val (16#FFFF#) then
			return In_32;
		else
			return In_16;
		end if;
	end Get_Bit;
	Shared_Num : array (Bit, Boolean) of Natural;
begin
	declare
		File : Ada.Text_IO.File_Type;
	begin
		Open (File, In_File, Argument (1));
		while not End_Of_File (File) loop
			declare
				Line : constant String := Get_Line (File);
				type Range_Type is record
					First : Positive;
					Last : Natural;
				end record;
				Fields : array (1 .. 14) of Range_Type;
				P : Positive := Line'First;
				N : Natural;
				Code : Wide_Wide_Character;
			begin
				for I in Fields'Range loop
					N := P;
					while N <= Line'Last and then Line (N) /= ';' loop
						N := N + 1;
					end loop;
					if (N <= Line'Last) /= (I < Field'Last) then
						raise Data_Error with Line & " -- A";
					end if;
					Fields (I).First := P;
					Fields (I).Last := N - 1;
					P := N + 1; -- skip ';'
				end loop;
				Code := Value (Line (Fields (1).First .. Fields (1).Last));
				if Fields (13).First <= Fields (13).Last then -- uppercase
					Insert (
						Upper_Table,
						Code,
						Value (Line (Fields (13).First .. Fields (13).Last)));
				end if;
				if Fields (14).First <= Fields (14).Last then -- lowercase
					Insert (
						Lower_Table,
						Code,
						Value (Line (Fields (14).First .. Fields (14).Last)));
				end if;
				-- note: last field (15) is titlecase.
			end;
		end loop;
		Close (File);
	end;
	Upper_Num := Natural (Length (Upper_Table));
	Lower_Num := Natural (Length (Lower_Table));
	declare
		I : WWC_Maps.Cursor := First (Lower_Table);
	begin
		while Has_Element (I) loop
			if Contains (Upper_Table, Element (I))
				and then Element (Upper_Table, Element (I)) = Key (I)
			then
				Insert (Shared_Table, Key (I), Element (I));
			end if;
			I := Next (I);
		end loop;
	end;
	for B in Bit loop
		for Compressed in Boolean loop
			Shared_Num (B, Compressed) := 0;
		end loop;
	end loop;
	declare
		I : WWC_Maps.Cursor := First (Shared_Table);
	begin
		while Has_Element (I) loop
			declare
				B : Bit := Get_Bit (Key (I));
			begin
				if Compressible (I) then
					declare
						K : Wide_Wide_Character := Key (I);
						E : Wide_Wide_Character := Element (I);
						N : WWC_Maps.Cursor := Next (I);
						RLE : Positive := 1;
						Compressed : Boolean;
					begin
						while Has_Element (N)
							and then RLE < 255
							and then Compressible (N)
							and then Key (N) = Wide_Wide_Character'Succ (K)
							and then Element (N) = Wide_Wide_Character'Succ (E)
						loop
							K := Key (N);
							E := Element (N);
							N := Next (N);
							RLE := RLE + 1;
						end loop;
						I := N;
						Compressed := RLE > 1;
						Shared_Num (B, Compressed) := Shared_Num (B, Compressed) + 1;
					end;
				else
					Shared_Num (B, False) := Shared_Num (B, False) + 1;
					I := Next (I);
				end if;
			end;
		end loop;
	end;
	Put_Line ("pragma License (Unrestricted);");
	Put_Line ("--  translated unit from UnicodeData.txt (13, 14)");
	Put_Line ("package Ada.UCD.Simple_Case_Mapping is");
	Put_Line ("   pragma Pure;");
	New_Line;
	Put ("   L_Total : constant := ");
	Put (Lower_Num, Width => 1);
	Put (";");
	New_Line;
	Put ("   U_Total : constant := ");
	Put (Upper_Num, Width => 1);
	Put (";");
	New_Line;
	New_Line;
	Put_Line ("   type Run_Length_8 is mod 2 ** 8;");
	New_Line;
	Put_Line ("   type Compressed_Item_Type is record");
	Put_Line ("      Start : UCS_2;");
	Put_Line ("      Length : Run_Length_8;");
	Put_Line ("      Diff : Difference_8;");
	Put_Line ("   end record;");
	Put_Line ("   pragma Suppress_Initialization (Compressed_Item_Type);");
	Put_Line ("   pragma Pack (Compressed_Item_Type);");
	Put_Line ("   pragma Compile_Time_Error (Compressed_Item_Type'Size /= 32, ""packed?"");");
	New_Line;
	Put_Line ("   type Compressed_Type is array (Positive range <>) of Compressed_Item_Type;");
	Put_Line ("   pragma Suppress_Initialization (Compressed_Type);");
	Put_Line ("   pragma Pack (Compressed_Type);");
	Put_Line ("   pragma Compile_Time_Error (Compressed_Type'Component_Size /= 32, ""packed?"");");
	New_Line;
	Put ("   subtype SL_Table_XXXX_Type is Map_16x1_Type (1 .. ");
	Put (Shared_Num (In_16, False), Width => 1);
	Put (");");
	New_Line;
	Put ("   subtype SL_Table_XXXX_Compressed_Type is Compressed_Type (1 .. ");
	Put (Shared_Num (In_16, True), Width => 1);
	Put (");");
	New_Line;
	Put ("   subtype SL_Table_1XXXX_Compressed_Type is Compressed_Type (1 .. ");
	Put (Shared_Num (In_32, True), Width => 1);
	Put (");");
	New_Line;
	if Shared_Num (In_32, False) /= 0 then
		raise Data_Error with "num of shared";
	end if;
	Put ("   subtype DL_Table_XXXX_Type is Map_16x1_Type (1 .. ");
	Put (Lower_Num - Natural (Length (Shared_Table)), Width => 1);
	Put (");");
	New_Line;
	Put ("   subtype DU_Table_XXXX_Type is Map_16x1_Type (1 .. ");
	Put (Upper_Num - Natural (Length (Shared_Table)), Width => 1);
	Put (");");
	New_Line;
	New_Line;
	for B in Bit loop
		for Compressed in Boolean loop
			if Shared_Num (B, Compressed) > 0 then
				Put ("   SL_Table_");
				if B = In_32 then
					Put ("1");
				end if;
				Put ("XXXX");
				if Compressed then
					Put ("_Compressed");
				end if;
				Put (" : constant SL_Table_");
				if B = In_32 then
					Put ("1");
				end if;
				Put ("XXXX");
				if Compressed then
					Put ("_Compressed");
				end if;
				Put ("_Type := (");
				New_Line;
				declare
					Offset : Integer := 0;
					I : WWC_Maps.Cursor := First (Shared_Table);
					Second : Boolean := False;
				begin
					if B = In_32 then
						Offset := 16#10000#;
					end if;
					while Has_Element (I) loop
						declare
							Item_B : Bit := Get_Bit (Key (I));
							Item_RLE : Positive := 1;
							N : WWC_Maps.Cursor := Next (I);
						begin
							if Compressible (I) then
								declare
									K : Wide_Wide_Character := Key (I);
									E : Wide_Wide_Character := Element (I);
								begin
									while Has_Element (N)
										and then Item_RLE < 255
										and then Compressible (N)
										and then Key (N) = Wide_Wide_Character'Succ (K)
										and then Element (N) = Wide_Wide_Character'Succ (E)
									loop
										K := Key (N);
										E := Element (N);
										N := Next (N);
										Item_RLE := Item_RLE + 1;
									end loop;
								end;
							end if;
							if Item_B = B
								and then (Item_RLE > 1) = Compressed
							then
								if Second then
									Put (",");
									New_Line;
								end if;
								Put ("      ");
								if Shared_Num (B, Compressed) = 1 then
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
										Wide_Wide_Character'Pos (Element (I))
										- Wide_Wide_Character'Pos (Key (I)),
										Width => 1);
								else
									Put (
										Integer'(Wide_Wide_Character'Pos (Element (I))) - Offset,
										Base => 16,
										Width => 8, -- 16#XXXX#
										Padding => '0');
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
	Put_Line ("   DL_Table_XXXX : constant DL_Table_XXXX_Type := (");
	declare
		I : WWC_Maps.Cursor := First (Lower_Table);
		Second : Boolean := False;
	begin
		while Has_Element (I) loop
			if not (
				Contains (Shared_Table, Key (I))
				and then Element (Shared_Table, Key (I)) = Element (I))
			then
				if Second then
					Put (",");
					New_Line;
				end if;
				Put ("      (");
				Put (
					Integer'(Wide_Wide_Character'Pos (Key (I))),
					Base => 16,
					Width => 8, -- 16#XXXX#
					Padding => '0');
				Put (", ");
				Put (
					Integer'(Wide_Wide_Character'Pos (Element (I))),
					Base => 16,
					Width => 8, -- 16#XXXX#
					Padding => '0');
				Put (")");
				Second := True;
			end if;
			I := Next (I);
		end loop;
		Put (");");
		New_Line;
	end;
	New_Line;
	Put_Line ("   DU_Table_XXXX : constant DU_Table_XXXX_Type := (");
	declare
		I : WWC_Maps.Cursor := First (Upper_Table);
		Second : Boolean := False;
	begin
		while Has_Element (I) loop
			if not (
				Contains (Shared_Table, Element (I))
				and then Element (Shared_Table, Element (I)) = Key (I))
			then
				if Second then
					Put (",");
					New_Line;
				end if;
				Put ("      (");
				Put (
					Integer'(Wide_Wide_Character'Pos (Key (I))),
					Base => 16,
					Width => 8, -- 16#XXXX#
					Padding => '0');
				Put (", ");
				Put (
					Integer'(Wide_Wide_Character'Pos (Element (I))),
					Base => 16,
					Width => 8, -- 16#XXXX#
					Padding => '0');
				Put (")");
				Second := True;
			end if;
			I := Next (I);
		end loop;
		Put (");");
		New_Line;
	end;
	New_Line;
	Put_Line ("end Ada.UCD.Simple_Case_Mapping;");
end ucd_simplecasemapping;
