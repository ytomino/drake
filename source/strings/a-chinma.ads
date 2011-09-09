pragma License (Unrestricted);
--  implementation unit
with System.Reference_Counting;
package Ada.Characters.Inside.Maps is
   pragma Pure;

   subtype Character_Type is Wide_Wide_Character;
   subtype Character_Sequence is Wide_Wide_String;

   type Character_Mapping (Length : Natural) is limited record
      Reference_Count : aliased System.Reference_Counting.Counter;
      From : Character_Sequence (1 .. Length); -- To_Domain
      To : Character_Sequence (1 .. Length); -- To_Range
   end record;
   pragma Suppress_Initialization (Character_Mapping);
   --  all object must have explicit initialization

   function Value (
      Map : not null access constant Character_Mapping;
      Element : Character_Type)
      return Character_Type;

   function Value (
      Map : not null access constant Character_Mapping;
      Element : Character)
      return Character;

   function To_Mapping (
      From, To : Character_Sequence;
      Initial_Reference_Count : System.Reference_Counting.Counter)
      return Character_Mapping;

   procedure Translate (
      Source : String;
      Mapping : not null access constant Character_Mapping;
      Item : out String; -- Source'Length * 6, at least
      Last : out Natural);

   --  for Equal_Case_Insensitive, Less_Case_Insensitive
   function Compare (
      Left : String;
      Right : String;
      Mapping : not null access constant Character_Mapping)
      return Integer;

private

   procedure Sort (From, To : in out Character_Sequence);
   procedure Sort (From, To : in out Character_Sequence; Last : out Natural);
   --  From'First = To'First

end Ada.Characters.Inside.Maps;
