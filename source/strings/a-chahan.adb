with Ada.Characters.Inside.Lower_Case_Maps;
with Ada.Characters.Inside.Upper_Case_Maps;
package body Ada.Characters.Handling is

   function To_Lower (Item : String) return String is
   begin
      return Inside.Translate (
         Item,
         Inside.Lower_Case_Maps.Lower_Case_Map);
   end To_Lower;

   function To_Upper (Item : String) return String is
   begin
      return Inside.Translate (
         Item,
         Inside.Upper_Case_Maps.Upper_Case_Map);
   end To_Upper;

   package body ASCII is

      function To_Upper (Item : Character) return Character is
      begin
         if Item in 'a' .. 'z' then
            return Character'Val (Character'Pos (Item) - 16#20#);
         else
            return Item;
         end if;
      end To_Upper;

   end ASCII;

end Ada.Characters.Handling;
