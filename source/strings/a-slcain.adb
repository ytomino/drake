with Ada.Strings.Naked_Maps.Case_Folding;
function Ada.Strings.Less_Case_Insensitive (Left, Right : String)
   return Boolean is
begin
   return Strings.Naked_Maps.Compare (
      Left,
      Right,
      Strings.Naked_Maps.Case_Folding.Case_Folding_Map.all) < 0;
end Ada.Strings.Less_Case_Insensitive;
