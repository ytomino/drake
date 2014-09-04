with Ada.Strings.Composites;
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
	begin
		Ada.Strings.Composites.Get_Combined (S, Last);
		pragma Assert (Last = 3);
		Ada.Strings.Composites.Get_Combined (S (Last + 1 .. S'Last), Last);
		pragma Assert (Last = 5);
		Ada.Strings.Composites.Get_Combined (S (Last + 1 .. S'Last), Last);
		pragma Assert (Last = 9);
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
	-- less
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'(""), ""));
	pragma Assert (Ada.Strings.Normalization.Less (WWS'(""), "A"));
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'("A"), ""));
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'("A"), "A"));
	pragma Assert (Ada.Strings.Normalization.Less (WWS'("A"), "AA"));
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'("AA"), "A"));
	pragma Assert (Ada.Strings.Normalization.Less (WWS'("AA"), "AB"));
	pragma Assert (not Ada.Strings.Normalization.Less (WWS'("AB"), "AA"));
	pragma Debug (Ada.Debug.Put ("OK"));
end normalize;
