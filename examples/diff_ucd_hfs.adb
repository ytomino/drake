-- compare two normalization tables
-- expected result:
-- -------------------------
-- decomposing maps:
--   size of UCD:        981
--   size of VFS:        970
--   only in UCD:   16#1B06#
--   only in UCD:   16#1B08#
--   only in UCD:   16#1B0A#
--   only in UCD:   16#1B0C#
--   only in UCD:   16#1B0E#
--   only in UCD:   16#1B12#
--   only in UCD:   16#1B3B#
--   only in UCD:   16#1B3D#
--   only in UCD:   16#1B40#
--   only in UCD:   16#1B41#
--   only in UCD:   16#1B43#
-- combining classes:
--   size of UCD:        602
--   size of VFS:        322
--   only in UCD:    16#358#
--   only in UCD:    16#359#
--   ...
--   only in UCD:   16#FE25#
--   only in UCD:   16#FE26#
-- -------------------------
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO;
with Ada.Strings.Composites; -- from Unicode Character Database
with Ada.Strings.Normalization;
with Ada.Text_IO;
with C.vfs_utfconvdata; -- from XNU
procedure diff_ucd_hfs is
	subtype WC is Wide_Character;
	subtype Decomposed is Wide_String (1 .. 2);
	package Decomposing_Maps is new Ada.Containers.Ordered_Maps (WC, Decomposed);
	package Combining_Class_Maps is new Ada.Containers.Ordered_Maps (WC, Integer);
	UCD_Decomposing_Map, VFS_Decomposing_Map : Decomposing_Maps.Map;
	UCD_Combining_Class_Map, VFS_Combining_Class_Map : Combining_Class_Maps.Map;
begin
	-- make decomposing map from UCD
	declare
		Target : Decomposing_Maps.Map renames UCD_Decomposing_Map;
		procedure Process (Pre : Wide_Wide_Character; De : Wide_Wide_String) is
			Second : Wide_Wide_Character;
		begin
			if De'Length = 2 then
				Second := De (De'First + 1);
			else
				Second := Wide_Wide_Character'Val (0);
			end if;
			if Wide_Wide_Character'Pos (Pre) <= 16#FFFF#
				and then De'Length in 1 .. 2
				and then Wide_Wide_Character'Pos (De (De'First)) <= 16#FFFF#
				and then Wide_Wide_Character'Pos (Second) <= 16#FFFF#
			then
				Decomposing_Maps.Insert (
					Target,
					WC'Val (Wide_Wide_Character'Pos (Pre)),
					Decomposed'(
						1 => WC'Val (Wide_Wide_Character'Pos (De (De'First))),
						2 => WC'Val (Wide_Wide_Character'Pos (Second))));
			end if;
		end Process;
	begin
		Ada.Strings.Normalization.Iterate (False, Process'Access);
	end;
	-- make decomposing map from VFS
	declare
		use type C.size_t;
		package V renames C.vfs_utfconvdata;
		use type V.u_int16_t;
		Target : Decomposing_Maps.Map renames VFS_Decomposing_Map;
		I : C.size_t := 0;
	begin
		while I < V.CFUniCharDecompositionTable'Length loop
			declare
				Key : V.u_int16_t := V.CFUniCharDecompositionTable (I);
				Value : V.u_int16_t := V.CFUniCharDecompositionTable (I + 1);
				-- take key apart
				Length : V.u_int16_t := (Value / 2 ** 12) and 7;
				Offset : V.u_int16_t := Value and 16#0FFF#;
				D : Decomposed;
			begin
				if Length = 1 then
					D (1) := WC'Val (Offset);
					D (2) := WC'Val (0);
				elsif Length = 2 then
					D (1) := WC'Val (V.CFUniCharMultipleDecompositionTable (C.size_t (Offset)));
					D (2) := WC'Val (V.CFUniCharMultipleDecompositionTable (C.size_t (Offset + 1)));
				else
					raise Program_Error;
				end if;
				Decomposing_Maps.Insert (Target, WC'Val (Key), D);
			end;
			I := I + 2;
		end loop;
	end;
	-- compare decomposing maps
	declare
		use Ada.Text_IO;
		use Ada.Integer_Text_IO;
		I : Decomposing_Maps.Cursor := UCD_Decomposing_Map.First;
		J : Decomposing_Maps.Cursor := VFS_Decomposing_Map.First;
	begin
		Put ("decomposing maps:");
		New_Line;
		Put ("  size of UCD:");
		Put (UCD_Decomposing_Map.Length);
		New_Line;
		Put ("  size of VFS:");
		Put (VFS_Decomposing_Map.Length);
		New_Line;
		while Decomposing_Maps.Has_Element (I) and then Decomposing_Maps.Has_Element (J) loop
			if Decomposing_Maps.Key (I) < Decomposing_Maps.Key (J) then
				Put ("  only in UCD:");
				Put (WC'Pos (Decomposing_Maps.Key (I)), Base => 16);
				New_Line;
				Decomposing_Maps.Next (I);
			elsif Decomposing_Maps.Key (I) > Decomposing_Maps.Key (J) then
				Put ("  only in VFS:");
				Put (WC'Pos (Decomposing_Maps.Key (J)), Base => 16);
				New_Line;
				Decomposing_Maps.Next (J);
			else
				if Decomposing_Maps.Element (I) /= Decomposing_Maps.Element (J) then
					Put ("  differ:");
					Put (WC'Pos (Decomposing_Maps.Key (I)), Base => 16);
					New_Line;
				end if;
				Decomposing_Maps.Next (I);
				Decomposing_Maps.Next (J);
			end if;
		end loop;
		while Decomposing_Maps.Has_Element (I) loop
			Put ("  only in UCD:");
			Put (WC'Pos (Decomposing_Maps.Key (I)), Base => 16);
			New_Line;
			Decomposing_Maps.Next (I);
		end loop;
		while Decomposing_Maps.Has_Element (J) loop
			Put ("  only in VFS:");
			Put (WC'Pos (Decomposing_Maps.Key (J)), Base => 16);
			New_Line;
			Decomposing_Maps.Next (J);
		end loop;
	end;
	-- make combining class map from UCD
	declare
		Target : Combining_Class_Maps.Map renames UCD_Combining_Class_Map;
		procedure Process (Item : Wide_Wide_Character; Combining_Class : Ada.Strings.Composites.Class) is
		begin
			if Wide_Wide_Character'Pos (Item) <= 16#FFFF# then
				Combining_Class_Maps.Insert (
					Target,
					WC'Val (Wide_Wide_Character'Pos (Item)),
					Integer (Combining_Class));
			end if;
		end Process;
	begin
		Ada.Strings.Composites.Iterate (Process'Access);
	end;
	-- make combining class map from VFS
	declare
		use type C.size_t;
		package V renames C.vfs_utfconvdata;
		use type V.u_int8_t;
		Target : Combining_Class_Maps.Map renames VFS_Combining_Class_Map;
	begin
		for I in C.size_t'(0) .. 16#FFFF# loop
			declare
				value : V.u_int8_t := V.CFUniCharCombiningPropertyBitmap (I / 2 ** 8);
			begin
				if value /= 0 then
					declare
						Combining_Class : constant V.u_int8_t :=
							V.CFUniCharCombiningPropertyBitmap (C.size_t (value) * 256 + (I and 16#FF#));
					begin
						if Combining_Class /= 0 then
							Combining_Class_Maps.Insert (Target, WC'Val (I), Integer (Combining_Class));
						end if;
					end;
				end if;
			end;
		end loop;
	end;
	-- compare combining classes
	declare
		use Ada.Text_IO;
		use Ada.Integer_Text_IO;
		I : Combining_Class_Maps.Cursor := UCD_Combining_Class_Map.First;
		J : Combining_Class_Maps.Cursor := VFS_Combining_Class_Map.First;
	begin
		Put ("combining classes:");
		New_Line;
		Put ("  size of UCD:");
		Put (UCD_Combining_Class_Map.Length);
		New_Line;
		Put ("  size of VFS:");
		Put (VFS_Combining_Class_Map.Length);
		New_Line;
		while Combining_Class_Maps.Has_Element (I) and then Combining_Class_Maps.Has_Element (J) loop
			if Combining_Class_Maps.Key (I) < Combining_Class_Maps.Key (J) then
				Put ("  only in UCD:");
				Put (WC'Pos (Combining_Class_Maps.Key (I)), Base => 16);
				New_Line;
				Combining_Class_Maps.Next (I);
			elsif Combining_Class_Maps.Key (I) > Combining_Class_Maps.Key (J) then
				Put ("  only in VFS:");
				Put (WC'Pos (Combining_Class_Maps.Key (J)), Base => 16);
				New_Line;
				Combining_Class_Maps.Next (J);
			else
				if Combining_Class_Maps.Element (I) /= Combining_Class_Maps.Element (J) then
					Put ("  differ:");
					Put (WC'Pos (Combining_Class_Maps.Key (I)), Base => 16);
					New_Line;
				end if;
				Combining_Class_Maps.Next (I);
				Combining_Class_Maps.Next (J);
			end if;
		end loop;
		while Combining_Class_Maps.Has_Element (I) loop
			Put ("  only in UCD:");
			Put (WC'Pos (Combining_Class_Maps.Key (I)), Base => 16);
			New_Line;
			Combining_Class_Maps.Next (I);
		end loop;
		while Combining_Class_Maps.Has_Element (J) loop
			Put ("  only in VFS:");
			Put (WC'Pos (Combining_Class_Maps.Key (J)), Base => 16);
			New_Line;
			Combining_Class_Maps.Next (J);
		end loop;
	end;
end diff_ucd_hfs;
