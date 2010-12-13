with Ada.Characters.Inside.Maps.Lower_Case;
with Ada.Characters.Inside.Maps.Upper_Case;
package body Ada.Characters.Handling is

   function To_Lower (Item : Character) return Character is
   begin
      return Inside.Maps.Value (
         Inside.Maps.Lower_Case.Lower_Case_Map,
         Item);
   end To_Lower;

   function To_Upper (Item : Character) return Character is
   begin
      return Inside.Maps.Value (
         Inside.Maps.Upper_Case.Upper_Case_Map,
         Item);
   end To_Upper;

   function To_Lower (Item : String) return String is
   begin
      return Inside.Maps.Translate (
         Item,
         Inside.Maps.Lower_Case.Lower_Case_Map);
   end To_Lower;

   function To_Upper (Item : String) return String is
   begin
      return Inside.Maps.Translate (
         Item,
         Inside.Maps.Upper_Case.Upper_Case_Map);
   end To_Upper;

end Ada.Characters.Handling;
