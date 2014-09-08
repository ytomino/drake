pragma License (Unrestricted);
--  implementation unit
with System.Reference_Counting;
package Ada.Strings.Naked_Maps is
   pragma Pure;

   --  Representation for a set of Wide_Wide_Character values

   subtype Character_Type is Wide_Wide_Character;
   subtype Character_Sequence is Wide_Wide_String;

   --  alternative conversions functions
   --    raising exception instead of using substitute.

   function To_Character (Item : Wide_Wide_Character)
      return Character;
   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character;

   --  sets

   type Character_Range is record
      Low : Character_Type;
      High : Character_Type;
   end record;
   pragma Suppress_Initialization (Character_Range);

   type Character_Ranges is array (Positive range <>) of Character_Range;
   pragma Suppress_Initialization (Character_Ranges);

   type Character_Set (Length : Natural) is limited record
      Reference_Count : aliased System.Reference_Counting.Counter;
      Items : aliased Character_Ranges (1 .. Length);
   end record;
   pragma Suppress_Initialization (Character_Set);

   --  place Reference_Count at first
   for Character_Set use record
      Reference_Count at 0 range
         0 ..
         System.Reference_Counting.Counter'Size - 1;
   end record;

   function Is_In (
      Element : Character_Type;
      Set : Character_Set)
      return Boolean;

   --  for Handling
   function Is_In (
      Element : Character;
      Set : Character_Set)
      return Boolean;

   --  making operations

   procedure Add (
      A : in out Character_Ranges;
      Last : in out Natural;
      L, H : Character_Type);

   procedure Merge (
      Target : out Character_Ranges;
      Last : out Natural;
      Left, Right : Character_Ranges);

   --  maps

   type Character_Mapping (Length : Natural) is limited record
      Reference_Count : aliased System.Reference_Counting.Counter;
      From : Character_Sequence (1 .. Length); -- To_Domain
      To : Character_Sequence (1 .. Length); -- To_Range
   end record;
   pragma Suppress_Initialization (Character_Mapping);

   --  place Reference_Count at first
   for Character_Mapping use record
      Reference_Count at 0 range
         0 ..
         System.Reference_Counting.Counter'Size - 1;
   end record;

   function To_Mapping (
      From, To : Character_Sequence;
      Initial_Reference_Count : System.Reference_Counting.Counter)
      return Character_Mapping;

   function Value (
      Map : Character_Mapping;
      Element : Character_Type)
      return Character_Type;

   --  for Handling
   function Value (
      Map : Character_Mapping;
      Element : Character)
      return Character;

   --  for Handling
   procedure Translate (
      Source : String;
      Mapping : Character_Mapping;
      Item : out String; -- Source'Length * 6, at least
      Last : out Natural);

private

   type Character_Ranges_Array is
      array (Positive range <>) of not null access constant Character_Ranges;
   pragma Suppress_Initialization (Character_Ranges_Array);

   --  for Set_Constants
   procedure Merge (
      Target : out Character_Ranges;
      Last : out Natural;
      Source : Character_Ranges_Array);

   procedure Sort (From, To : in out Character_Sequence);
   procedure Sort (From, To : in out Character_Sequence; Last : out Natural);
   --  From'First = To'First

end Ada.Strings.Naked_Maps;
