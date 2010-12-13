with Ada.Characters.Inside.Maps.Lower_Case;
function Ada.Strings.Equal_Case_Insensitive (Left, Right : String)
   return Boolean is
begin
   return Left'Length = Right'Length
      and then
         Characters.Inside.Maps.Compare (
            Left,
            Right,
            Characters.Inside.Maps.Lower_Case.Lower_Case_Map) = 0;
end Ada.Strings.Equal_Case_Insensitive;
