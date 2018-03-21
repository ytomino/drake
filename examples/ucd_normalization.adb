-- convert UCD/UnicodeData.txt, UCD/CompositionExclusions.txt
-- bin/ucd_normalization -r $UCD/UnicodeData.txt $UCD/CompositionExclusions.txt > ../source/strings/a-ucdnor.ads
-- bin/ucd_normalization -u $UCD/UnicodeData.txt $UCD/CompositionExclusions.txt > ../source/strings/a-ucnoun.ads
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
procedure ucd_normalization is
	function Value (S : String) return Wide_Wide_Character is
		Img : constant String := "Hex_" & (1 .. 8 - S'Length => '0') & S;
	begin
		return Wide_Wide_Character'Value (Img);
	end Value;
	procedure Put_16 (Item : Integer) is
	begin
		if Item >= 16#10000# then
			Put (Item, Width => 1, Base => 16);
		else
			declare
				S : String (1 .. 8); -- "16#XXXX#"
			begin
				Put (S, Item, Base => 16);
				S (1) := '1';
				S (2) := '6';
				S (3) := '#';
				for I in reverse 4 .. 6 loop
					if S (I) = '#' then
						S (4 .. I) := (others => '0');
						exit;
					end if;
				end loop;
				Put (S);
			end;
		end if;
	end Put_16;
	function NFS_Exclusion (C : Wide_Wide_Character) return Boolean is
	begin
		case Wide_Wide_Character'Pos (C) is
			when 16#2000# .. 16#2FFF#
				| 16#F900# .. 16#FAFF#
				| 16#2F800# .. 16#2FAFF# =>
				return True;
			when others =>
				return False;
		end case;
	end NFS_Exclusion;
	package Decomposite_Maps is new Ada.Containers.Ordered_Maps (
		Wide_Wide_Character,
		Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);
	use Decomposite_Maps;
	package WWC_Sets is new Ada.Containers.Ordered_Sets (
		Wide_Wide_Character);
	use WWC_Sets;
	NFD : Decomposite_Maps.Map;
	Exclusions : WWC_Sets.Set;
	type Kind_Type is (Decomposition, Excluded, Singleton);
	type Bit is (In_16, In_32);
	function Get_Bit (C : Wide_Wide_Character) return Bit is
	begin
		if C > Wide_Wide_Character'Val (16#FFFF#) then
			return In_32;
		else
			return In_16;
		end if;
	end Get_Bit;
	function Get_Bit (S : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) return Bit is
	begin
		for I in 1 .. Length (S) loop
			if Get_Bit (Element (S, I)) = In_32 then
				return In_32;
			end if;
		end loop;
		return In_16;
	end Get_Bit;
	type Normalization is (D, C);
	Total_Num : array (Boolean, Normalization) of Natural;
	Num : array (Boolean, Kind_Type, Bit) of Natural;
	type Output_Kind is (Reversible, Unreversible);
	Output : Output_Kind;
begin
	if Argument (1) = "-r" then
		Output := Reversible;
	elsif Argument (1) = "-u" then
		Output := Unreversible;
	else
		raise Data_Error with "-r or -u";
	end if;
	declare
		File : Ada.Text_IO.File_Type;
	begin
		Open (File, In_File, Argument (2));
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
				Token_First : Positive;
				Token_Last : Natural;
				Code : Wide_Wide_Character;
				Alt : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
			begin
				for I in Fields'Range loop
					N := P;
					while N <= Line'Last and then Line (N) /= ';' loop
						N := N + 1;
					end loop;
					if (N <= Line'Last) /= (I < Field'Last) then
						raise Data_Error with Line & " -- 2A";
					end if;
					Fields (I).First := P;
					Fields (I).Last := N - 1;
					P := N + 1; -- skip ';'
				end loop;
				Code := Value (Line (Fields (1).First .. Fields (1).Last));
				if Fields (6).First <= Fields (6).Last then -- normalization
					if Line (Fields (6).First) = '<' then
						null; -- skip NFKD
					else -- NFD
						Alt := Null_Unbounded_Wide_Wide_String;
						P := Fields (6).First;
						while P <= Fields (6).Last loop
							Find_Token (
								Line (P .. Fields (6).Last),
								Hexadecimal_Digit_Set,
								Inside,
								Token_First,
								Token_Last);
							if Token_First /= P then
								raise Data_Error with Line & " -- 2B";
							end if;
							Append (Alt, Value (Line (Token_First .. Token_Last)));
							P := Token_Last + 1;
							exit when P > Fields (6).Last;
							N := Index_Non_Blank (Line (P .. Fields (6).Last));
							if N = 0 then
								raise Data_Error with Line & " -- 2C";
							end if;
							P := N;
						end loop;
						Insert (NFD, Code, Alt);
					end if;
				end if;
			end;
		end loop;
		Close (File);
	end;
	declare
		I : Decomposite_Maps.Cursor := First (NFD);
	begin
		while Has_Element (I) loop
			if not NFS_Exclusion (Key (I)) then
				declare
					J : Decomposite_Maps.Cursor := Next (I);
				begin
					while Has_Element (J) loop
						if Element (I) = Element (J) then
							raise Data_Error with "dup";
						end if;
						J := Next (J);
					end loop;
				end;
			end if;
			I := Next (I);
		end loop;
	end;
	declare
		File : Ada.Text_IO.File_Type;
	begin
		Open (File, In_File, Argument (3));
		while not End_Of_File (File) loop
			declare
				Line : constant String := Get_Line (File);
				P : Positive := Line'First;
				Token_First : Positive;
				Token_Last : Natural;
				First : Wide_Wide_Character;
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
						raise Data_Error with Line & " -- 3A";
					end if;
					First := Value (Line (Token_First .. Token_Last));
					P := Token_Last + 1;
					if Line (P) = '.' then
						raise Data_Error with Line & " -- 3B";
					end if;
					if not Contains (NFD, First) then
						raise Data_Error with Line & " -- 3C";
					end if;
					Insert (Exclusions, First);
				end if;
			end;
		end loop;
		Close (File);
	end;
	-- # (4) Non-Starter Decompositions
	-- # 0344                 COMBINING GREEK DIALYTIKA TONOS
	-- # 0F73                 TIBETAN VOWEL SIGN II
	-- # 0F75                 TIBETAN VOWEL SIGN UU
	-- # 0F81                 TIBETAN VOWEL SIGN REVERSED II
	Insert (Exclusions, Wide_Wide_Character'Val (16#0344#));
	Insert (Exclusions, Wide_Wide_Character'Val (16#0F73#));
	Insert (Exclusions, Wide_Wide_Character'Val (16#0F75#));
	Insert (Exclusions, Wide_Wide_Character'Val (16#0F81#));
	-- count
	for NFSE in Boolean loop
		for K in Kind_Type loop
			for B in Bit loop
				Num (NFSE, K, B) := 0;
			end loop;
		end loop;
		for N in Normalization loop
			Total_Num (NFSE, N) := 0;
		end loop;
	end loop;
	declare
		I : Decomposite_Maps.Cursor := First (NFD);
	begin
		while Has_Element (I) loop
			declare
				NFSE : Boolean := NFS_Exclusion (Key (I));
				B : Bit := Bit'Max (Get_Bit (Key (I)), Get_Bit (Element (I)));
				K : Kind_Type;
			begin
				if Contains (Exclusions, Key (I)) then
					K := Excluded;
				elsif Length (Element (I)) > 1 then
					K := Decomposition;
					Total_Num (NFSE, C) := Total_Num (NFSE, C) + 1;
				else
					K := Singleton;
				end if;
				Num (NFSE, K, B) := Num (NFSE, K, B) + 1;
				Total_Num (NFSE, D) := Total_Num (NFSE, D) + 1;
			end;
			I := Next (I);
		end loop;
	end;
	-- output the Ada spec
	case Output is
		when Reversible =>
			Put_Line ("pragma License (Unrestricted);");
			Put_Line ("--  implementation unit,");
			Put_Line ("--    translated from UnicodeData.txt (6), CompositionExclusions.txt");
			Put_Line ("package Ada.UCD.Normalization is");
			Put_Line ("   pragma Pure;");
			New_Line;
			Put_Line ("   --  excluding U+2000..U+2FFF, U+F900..U+FAFF, and U+2F800..U+2FAFF");
			New_Line;
			Put ("   NFD_Total : constant := ");
			Put (Total_Num (False, D), Width => 1);
			Put (";");
			New_Line;
			Put ("   NFC_Total : constant := ");
			Put (Total_Num (False, C), Width => 1);
			Put (";");
			New_Line;
			New_Line;
		when Unreversible =>
			Put_Line ("pragma License (Unrestricted);");
			Put_Line ("--  implementation unit,");
			Put_Line ("--    translated from UnicodeData.txt (6), CompositionExclusions.txt");
			Put_Line ("package Ada.UCD.Normalization.Unreversible is");
			Put_Line ("   pragma Pure;");
			New_Line;
			Put_Line ("   --  including U+2000..U+2FFF, U+F900..U+FAFF, and U+2F800..U+2FAFF");
			New_Line;
			Put ("   NFD_Unreversible_Total : constant := ");
			Put (Total_Num (True, D), Width => 1);
			Put (";");
			New_Line;
			Put ("   NFC_Unreversible_Total : constant := ");
			Put (Total_Num (True, C), Width => 1);
			Put (";");
			New_Line;
			New_Line;
	end case;
	declare
		NFSE : constant Boolean := Output = Unreversible;
	begin
		for K in Kind_Type loop
			for B in Bit loop
				if Num (NFSE, K, B) /= 0 then
					Put ("   NFD_");
					if NFSE then
						Put ("Unreversible_");
					end if;
					case K is
						when Decomposition => Put ("D_");
						when Excluded => Put ("E_");
						when Singleton => Put ("S_");
					end case;
					Put ("Table_");
					case B is
						when In_16 => Put ("XXXX");
						when In_32 => Put ("XXXXXXXX");
					end case;
					Put (" : constant Map_");
					case B is
						when In_16 => Put ("16");
						when In_32 => Put ("32");
					end case;
					Put ("x");
					case K is
						when Decomposition | Excluded => Put ("2");
						when Singleton => Put ("1");
					end case;
					Put ("_Type (1 .. ");
					Put (Num (NFSE, K, B), Width => 1);
					Put (") := (");
					New_Line;
					declare
						I : Decomposite_Maps.Cursor := First (NFD);
						Second : Boolean := False;
					begin
						while Has_Element (I) loop
							declare
								Item_NFSE : Boolean := NFS_Exclusion (Key (I));
								Item_B : Bit := Bit'Max (Get_Bit (Key (I)), Get_Bit (Element (I)));
								Item_K : Kind_Type;
							begin
								if Contains (Exclusions, Key (I)) then
									Item_K := Excluded;
								elsif Length (Element (I)) > 1 then
									Item_K := Decomposition;
								else
									Item_K := Singleton;
								end if;
								if Item_NFSE = NFSE
									and then Item_K = K
									and then Item_B = B
								then
									if Second then
										Put (",");
										New_Line;
									end if;
									Put ("      ");
									if Num (NFSE, K, B) = 1 then
										Put ("1 => ");
									end if;
									Put ("(");
									Put_16 (Wide_Wide_Character'Pos (Key (I)));
									Put (", ");
									if K = Singleton then
										Put_16 (
											Wide_Wide_Character'Pos (
												Element (Element (I), 1)));
									else
										declare
											E : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String
												renames Element (I);
										begin
											Put ("(");
											for EI in 1 .. Length (E) loop
												if EI > 1 then
													Put (", ");
												end if;
												Put_16 (
													Wide_Wide_Character'Pos (
														Element (E, EI)));
											end loop;
											Put (")");
										end;
									end if;
									Put (")");
									Second := True;
								end if;
							end;
							I := Next (I);
						end loop;
						Put (");");
						New_Line;
					end;
					New_Line;
				end if;
			end loop;
		end loop;
	end;
	case Output is
		when Reversible =>
			Put_Line ("end Ada.UCD.Normalization;");
		when Unreversible =>
			Put_Line ("end Ada.UCD.Normalization.Unreversible;");
	end case;
end ucd_normalization;
