with Ada.Characters.Conversions;
package body Ada.Strings.Wide_Maps is

   --  functions in Ada.Characters.Conversions uses substitute
   --  instead of raising exception,
   --  functions in this package do not have substitute parameter.

   function To_Wide_Character (Item : Wide_Wide_Character)
      return Wide_Character;
   function To_Wide_Character (Item : Wide_Wide_Character)
      return Wide_Character is
   begin
      if Wide_Wide_Character'Pos (Item) >= 16#ffff# then
         raise Constraint_Error;
      else
         return Wide_Character'Val (Wide_Wide_Character'Pos (Item));
      end if;
   end To_Wide_Character;

   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character;
   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character is
   begin
      case Wide_Character'Pos (Item) is
         when 16#d800# .. 16#dfff# =>
            raise Constraint_Error;
         when others =>
            return Wide_Wide_Character'Val (Wide_Character'Pos (Item));
      end case;
   end To_Wide_Wide_Character;

   function Identity return Wide_Character_Mapping is
   begin
      return Wide_Character_Mapping (Wide_Wide_Maps.Identity);
   end Identity;

   function Is_In (Element : Wide_Character; Set : Wide_Character_Set)
      return Boolean is
   begin
      return Wide_Wide_Maps.Is_In (
         To_Wide_Wide_Character (Element),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Set));
   end Is_In;

   function Is_Subset (Elements : Wide_Character_Set; Set : Wide_Character_Set)
      return Boolean is
   begin
      return Wide_Wide_Maps.Is_Subset (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Elements),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Set));
   end Is_Subset;

   function Null_Set return Wide_Character_Set is
   begin
      return Wide_Character_Set (Wide_Wide_Maps.Null_Set);
   end Null_Set;

   function To_Domain (Map : Wide_Character_Mapping)
      return Wide_Character_Sequence is
   begin
      return Characters.Conversions.To_Wide_String (Wide_Wide_Maps.To_Domain (
         Wide_Wide_Maps.Wide_Wide_Character_Mapping (Map)));
   end To_Domain;

   function To_Mapping (From, To : Wide_Character_Sequence)
      return Wide_Character_Mapping is
   begin
      return Wide_Character_Mapping (Wide_Wide_Maps.To_Mapping (
         Characters.Conversions.To_Wide_Wide_String (From),
         Characters.Conversions.To_Wide_Wide_String (To)));
   end To_Mapping;

   function To_Sequence (Set : Wide_Character_Set)
      return Wide_Character_Sequence is
   begin
      return Characters.Conversions.To_Wide_String (
         Wide_Wide_Maps.To_Sequence (
            Wide_Wide_Maps.Wide_Wide_Character_Set (Set)));
   end To_Sequence;

   function To_Set (Ranges : Wide_Character_Ranges)
      return Wide_Character_Set
   is
      WR : Wide_Wide_Maps.Wide_Wide_Character_Ranges (Ranges'Range);
   begin
      for I in Ranges'Range loop
         WR (I).Low := To_Wide_Wide_Character (Ranges (I).Low);
         WR (I).High := To_Wide_Wide_Character (Ranges (I).High);
      end loop;
      return Wide_Character_Set (Wide_Wide_Maps.To_Set (WR));
   end To_Set;

   function To_Set (Span : Wide_Character_Range) return Wide_Character_Set is
   begin
      return Wide_Character_Set (Wide_Wide_Maps.To_Set (
         Wide_Wide_Maps.Wide_Wide_Character_Range'(
            Low => To_Wide_Wide_Character (Span.Low),
            High => To_Wide_Wide_Character (Span.High))));
   end To_Set;

   function To_Set (Sequence : Wide_Character_Sequence)
      return Wide_Character_Set is
   begin
      return Wide_Character_Set (Wide_Wide_Maps.To_Set (
         Characters.Conversions.To_Wide_Wide_String (Sequence)));
   end To_Set;

   function To_Set (Singleton : Wide_Character) return Wide_Character_Set is
   begin
      return Wide_Character_Set (Wide_Wide_Maps.To_Set (
         To_Wide_Wide_Character (Singleton)));
   end To_Set;

   function To_Range (Map : Wide_Character_Mapping)
      return Wide_Character_Sequence is
   begin
      return Characters.Conversions.To_Wide_String (Wide_Wide_Maps.To_Range (
         Wide_Wide_Maps.Wide_Wide_Character_Mapping (Map)));
   end To_Range;

   function Value (Map : Wide_Character_Mapping; Element : Wide_Character)
      return Wide_Character is
   begin
      return To_Wide_Character (Wide_Wide_Maps.Value (
         Wide_Wide_Maps.Wide_Wide_Character_Mapping (Map),
         To_Wide_Wide_Character (Element)));
   end Value;

   function "=" (Left, Right : Wide_Character_Set) return Boolean is
   begin
      return Wide_Wide_Maps."=" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right));
   end "=";

   function "not" (Right : Wide_Character_Set) return Wide_Character_Set is
   begin
      return Wide_Character_Set (Wide_Wide_Maps."not" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "not";

   function "and" (Left, Right : Wide_Character_Set)
      return Wide_Character_Set is
   begin
      return Wide_Character_Set (Wide_Wide_Maps."and" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "and";

   function "or" (Left, Right : Wide_Character_Set)
      return Wide_Character_Set is
   begin
      return Wide_Character_Set (Wide_Wide_Maps."or" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "or";

   function "xor" (Left, Right : Wide_Character_Set)
      return Wide_Character_Set is
   begin
      return Wide_Character_Set (Wide_Wide_Maps."xor" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "xor";

   function "-" (Left, Right : Wide_Character_Set)
      return Wide_Character_Set is
   begin
      return Wide_Character_Set (Wide_Wide_Maps."and" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "-";

end Ada.Strings.Wide_Maps;
