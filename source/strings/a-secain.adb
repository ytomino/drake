with Ada.Characters.Inside.Maps.Case_Folding;
function Ada.Strings.Equal_Case_Insensitive (Left, Right : String)
   return Boolean
is
   pragma Suppress (All_Checks);
begin
   return Left'Length = Right'Length
      and then
         Characters.Inside.Maps.Compare (
            Left,
            Right,
            Characters.Inside.Maps.Case_Folding.Case_Folding_Map) = 0;
end Ada.Strings.Equal_Case_Insensitive;
