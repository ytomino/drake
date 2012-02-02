with Ada.Characters.Inside.Maps.Case_Folding;
function Ada.Strings.Less_Case_Insensitive (Left, Right : String)
   return Boolean is
begin
   return Characters.Inside.Maps.Compare (
      Left,
      Right,
      Characters.Inside.Maps.Case_Folding.Case_Folding_Map) < 0;
end Ada.Strings.Less_Case_Insensitive;
