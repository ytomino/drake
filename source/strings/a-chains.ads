pragma License (Unrestricted);
--  implementation package
with Interfaces;
package Ada.Characters.Inside is
   pragma Pure;

   subtype Character_Type is Wide_Wide_Character;
   subtype Character_Sequence is Wide_Wide_String;

   type Character_Mapping (Length : Natural) is limited record
      Reference_Count : aliased Interfaces.Integer_32; --  -1 as constant
      From : Character_Sequence (1 .. Length); --  To_Domain
      To : Character_Sequence (1 .. Length); --  To_Range
   end record;
   pragma Suppress_Initialization (Character_Mapping);
   --  all object must have explicit initialization

   function Value (
      Map : not null access constant Character_Mapping;
      Element : Character_Type)
      return Character_Type;

   use type Interfaces.Integer_32;

   function To_Mapping (
      From, To : Character_Sequence;
      Initial_Reference_Count : Interfaces.Integer_32 := -1)
      return Character_Mapping;

   procedure Translate (
      Source : String;
      Mapping : not null access constant Character_Mapping;
      Item : out String; --  Source'Length * 6, at least
      Last : out Natural);

   function Translate (
      Source : String;
      Mapping : not null access constant Character_Mapping)
      return String;

   function Compare (
      Left : String;
      Right : String;
      Mapping : not null access constant Character_Mapping)
      return Integer;

private

   procedure Sort (Map : not null access Character_Mapping);

end Ada.Characters.Inside;
