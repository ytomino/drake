with Ada.Characters.Latin_1;
with Ada.Strings.Composites;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Normalization;
procedure normalize is
	use type Ada.Strings.Composites.Class;
	subtype C is Character;
	subtype WWC is Wide_Wide_Character;
	subtype WWS is Wide_Wide_String;
begin
	-- combining class
	pragma Assert (Ada.Strings.Composites.Combining_Class (WWC'First) = 0);
	pragma Assert (Ada.Strings.Composites.Combining_Class (WWC'Val (16#0308#)) = 230);
	pragma Assert (Ada.Strings.Composites.Combining_Class (WWC'Val (16#0323#)) = 220);
	pragma Assert (Ada.Strings.Composites.Combining_Class (WWC'Val (16#3099#)) = 8);
	pragma Assert (Ada.Strings.Composites.Combining_Class (WWC'Val (16#e0100#)) = 0); -- VARIATION SELECTOR
	pragma Assert (Ada.Strings.Composites.Combining_Class (WWC'Last) = 0);
	declare
		S : constant Wide_Wide_String :=
			"a" & WWC'Val (16#0323#) & WWC'Val (16#0308#)
			& WWC'Val (16#304b#) & WWC'Val (16#3099#) -- ka & dakuten
			& "e" & WWC'Val (16#0323#) & WWC'Val (16#0304#) & WWC'Val (16#0301#);
		Last : Natural;
		Is_Illegal_Sequence : Boolean;
	begin
		Ada.Strings.Composites.Get_Combined (S, Last, Is_Illegal_Sequence);
		pragma Assert (Last = 3);
		pragma Assert (not Is_Illegal_Sequence);
		Ada.Strings.Composites.Get_Combined (S (Last + 1 .. S'Last), Last, Is_Illegal_Sequence);
		pragma Assert (Last = 5);
		pragma Assert (not Is_Illegal_Sequence);
		Ada.Strings.Composites.Get_Combined (S (Last + 1 .. S'Last), Last, Is_Illegal_Sequence);
		pragma Assert (Last = 9);
		pragma Assert (not Is_Illegal_Sequence);
	end;
	-- decomposing and composing
	pragma Assert (Ada.Strings.Normalization.Decompose (WWS'("")) = "");
	pragma Assert (Ada.Strings.Normalization.Decompose (WWS'("A")) = "A");
	pragma Assert (Ada.Strings.Normalization.Decompose ((1 => WWC'Val (16#304c#))) = WWC'Val (16#304b#) & WWC'Val (16#3099#));
	pragma Assert (Ada.Strings.Normalization.Decompose ((1 => WWC'Val (16#0390#))) = WWC'Val (16#03B9#) & WWC'Val (16#0308#) & WWC'Val (16#0301#));
	pragma Assert (Ada.Strings.Normalization.Decompose ((1 => WWC'Val (16#0958#))) = WWC'Val (16#0915#) & WWC'Val (16#093C#));
	pragma Assert (Ada.Strings.Normalization.Decompose ((1 => WWC'Val (16#AC00#))) = WWC'Val (16#1100#) & WWC'Val (16#1161#)); -- Hangul LV
	pragma Assert (Ada.Strings.Normalization.Decompose ((1 => WWC'Val (16#AC01#))) = WWC'Val (16#1100#) & WWC'Val (16#1161#) & WWC'Val (16#11A8#)); -- Hangul LVT
	pragma Assert (Ada.Strings.Normalization.Compose (WWS'("")) = "");
	pragma Assert (Ada.Strings.Normalization.Compose (WWS'("A")) = "A");
	pragma Assert (Ada.Strings.Normalization.Compose (WWC'Val (16#304b#) & WWC'Val (16#3099#)) = (1 => WWC'Val (16#304c#)));
	pragma Assert (Ada.Strings.Normalization.Compose (WWC'Val (16#03B9#) & WWC'Val (16#0308#) & WWC'Val (16#0301#)) = (1 => WWC'Val (16#0390#)));
	pragma Assert (Ada.Strings.Normalization.Compose (WWC'Val (16#0915#) & WWC'Val (16#093C#)) = WWC'Val (16#0915#) & WWC'Val (16#093C#)); -- composition exclusion
	pragma Assert (Ada.Strings.Normalization.Compose (WWC'Val (16#1100#) & WWC'Val (16#1161#)) = (1 => WWC'Val (16#AC00#))); -- Hangul LV
	pragma Assert (Ada.Strings.Normalization.Compose (WWC'Val (16#1100#) & WWC'Val (16#1161#) & WWC'Val (16#11A8#)) = (1 => WWC'Val (16#AC01#))); -- Hangul LVT
	pragma Assert (Ada.Strings.Normalization.Decompose (C'Val (16#e3#) & C'Val (16#81#) & C'Val (16#8c#)) = (C'Val (16#e3#) & C'Val (16#81#) & C'Val (16#8b#) & C'Val (16#e3#) & C'Val (16#82#) & C'Val (16#99#))); -- UTF-8
	-- equal
	pragma Assert (Ada.Strings.Normalization.Equal (WWS'(""), ""));
	pragma Assert (not Ada.Strings.Normalization.Equal (WWS'(""), "A"));
	pragma Assert (not Ada.Strings.Normalization.Equal (WWS'("A"), ""));
	pragma Assert (Ada.Strings.Normalization.Equal (WWS'("A"), "A"));
	pragma Assert (not Ada.Strings.Normalization.Equal (WWS'("A"), "AA"));
	pragma Assert (not Ada.Strings.Normalization.Equal (WWS'("AA"), "A"));
	pragma Assert (not Ada.Strings.Normalization.Equal (WWS'("AA"), "AB"));
	pragma Assert (not Ada.Strings.Normalization.Equal (WWS'("AB"), "AA"));
	pragma Assert (not Ada.Strings.Normalization.Equal ((1 => WWC'Val (16#00C0#)), 'A' & WWC'Val (16#0301#)));
	pragma Assert (Ada.Strings.Normalization.Equal ((1 => WWC'Val (16#00C1#)), 'A' & WWC'Val (16#0301#)));
	pragma Assert (Ada.Strings.Normalization.Equal ((1 => WWC'Val (16#304c#)), WWC'Val (16#304b#) & WWC'Val (16#3099#)));
	pragma Assert (Ada.Strings.Normalization.Equal (WWC'Val (16#304b#) & WWC'Val (16#3099#), (1 => WWC'Val (16#304c#))));
	-- less
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'(""), ""));
	pragma Assert (Ada.Strings.Normalization.Less (WWS'(""), "A"));
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'("A"), ""));
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'("A"), "A"));
	pragma Assert (Ada.Strings.Normalization.Less (WWS'("A"), "AA"));
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'("AA"), "A"));
	pragma Assert (Ada.Strings.Normalization.Less (WWS'("AA"), "AB"));
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'("AB"), "AA"));
	pragma Assert (Ada.Strings.Normalization.Less ((1 => WWC'Val (16#00C0#)), 'A' & WWC'Val (16#0301#)));
	pragma Assert (not Ada.Strings.Normalization.Less ((1 => WWC'Val (16#00C1#)), 'A' & WWC'Val (16#0301#)));
	pragma Assert (not Ada.Strings.Normalization.Less ((1 => WWC'Val (16#304c#)), WWC'Val (16#304b#) & WWC'Val (16#3099#)));
	pragma Assert (not Ada.Strings.Normalization.Less (WWC'Val (16#304b#) & WWC'Val (16#3099#), (1 => WWC'Val (16#304c#))));
	-- To_Base
	declare
		package Latin_1 renames Ada.Characters.Latin_1;
		package F renames Ada.Strings.Fixed;
		package M renames Ada.Strings.Maps;
	begin
		pragma Assert (M.Value (M.Constants.Basic_Map, '6') = '6'); -- 16#36#
		pragma Assert (M.Value (M.Constants.Basic_Map, 'Y') = 'Y'); -- 16#59#
		pragma Assert (M.Value (M.Constants.Basic_Map, Latin_1.LC_R) = 'r'); -- 16#72#
		pragma Assert (F.Translate (Latin_1.UC_O_Acute, M.Constants.Basic_Map) = "O"); -- 16#D4#
		pragma Assert (F.Translate (Latin_1.UC_O_Tilde, M.Constants.Basic_Map) = "O"); -- 16#D6#
		pragma Assert (F.Translate (Latin_1.UC_U_Grave, M.Constants.Basic_Map) = "U"); -- 16#D9#
		pragma Assert (F.Translate (Latin_1.UC_U_Acute, M.Constants.Basic_Map) = "U"); -- 16#DA#
		pragma Assert (F.Translate (Latin_1.LC_A_Circumflex, M.Constants.Basic_Map) = "a"); -- 16#E2#
		pragma Assert (F.Translate (Latin_1.LC_A_Tilde, M.Constants.Basic_Map) = "a"); -- 16#E3#
		pragma Assert (F.Translate (Latin_1.LC_E_Grave, M.Constants.Basic_Map) = "e"); -- 16#E8#
		pragma Assert (F.Translate (Latin_1.LC_E_Acute, M.Constants.Basic_Map) = "e"); -- 16#E9#
		pragma Assert (F.Translate (Latin_1.LC_I_Circumflex, M.Constants.Basic_Map) = "i"); -- 16#EE#
		pragma Assert (F.Translate (Latin_1.LC_I_Diaeresis, M.Constants.Basic_Map) = "i"); -- 16#EF#
		pragma Assert (F.Translate (Latin_1.LC_Y_Acute, M.Constants.Basic_Map) = "y"); -- 16#FD#
		pragma Assert (F.Translate (Latin_1.LC_Y_Diaeresis, M.Constants.Basic_Map) = "y"); -- 16#FF#
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end normalize;
