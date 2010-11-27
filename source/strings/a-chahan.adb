with Ada.Characters.Inside.Lower_Case_Maps;
with Ada.Characters.Inside.Upper_Case_Maps;
package body Ada.Characters.Handling is

   function To_Lower (Item : Character) return Character is
   begin
      return Inside.Value (
         Inside.Lower_Case_Maps.Lower_Case_Map,
         Item);
   end To_Lower;

   function To_Upper (Item : Character) return Character is
   begin
      return Inside.Value (
         Inside.Upper_Case_Maps.Upper_Case_Map,
         Item);
   end To_Upper;

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

end Ada.Characters.Handling;
