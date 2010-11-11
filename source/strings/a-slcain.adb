with Ada.Characters.Inside.Lower_Case_Maps;
function Ada.Strings.Less_Case_Insensitive (Left, Right : String)
   return Boolean is
begin
   return Characters.Inside.Compare (
      Left,
      Right,
      Characters.Inside.Lower_Case_Maps.Lower_Case_Map) < 0;
end Ada.Strings.Less_Case_Insensitive;
