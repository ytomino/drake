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
   function To_Wide_Character (Item : Wide_Wide_Character)
      return Wide_Character;
   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character;
   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character;

   --  sets

   type Character_Range is record
      Low : Character_Type;
      High : Character_Type;
   end record;
   pragma Suppress_Initialization (Character_Range);

   type Character_Ranges is array (Positive range <>) of Character_Range;
   pragma Suppress_Initialization (Character_Ranges);

   type Character_Set_Data (Length : Natural) is limited record
      Reference_Count : aliased System.Reference_Counting.Counter;
      Items : aliased Character_Ranges (1 .. Length);
   end record;
   pragma Suppress_Initialization (Character_Set_Data);

   --  place Reference_Count at first
   for Character_Set_Data use record
      Reference_Count at 0 range
         0 ..
         System.Reference_Counting.Counter'Size - 1;
   end record;

   type Character_Set_Access is access all Character_Set_Data;
   for Character_Set_Access'Storage_Size use 0;

   function Is_In (Element : Character_Type; Set : Character_Set_Data)
      return Boolean;

   --  making operations

   procedure Add (
      A : in out Character_Ranges;
      Last : in out Natural;
      L, H : Character_Type);

   --  "and"
   procedure Intersection (
      Result : out Character_Ranges;
      Last : out Natural;
      Left, Right : Character_Ranges);

   --  "or"
   procedure Union (
      Target : out Character_Ranges;
      Last : out Natural;
      Left, Right : Character_Ranges);

   --  maps

   type Character_Mapping_Data (Length : Natural) is limited record
      Reference_Count : aliased System.Reference_Counting.Counter;
      From : Character_Sequence (1 .. Length); -- To_Domain
      To : Character_Sequence (1 .. Length); -- To_Range
   end record;
   pragma Suppress_Initialization (Character_Mapping_Data);

   --  place Reference_Count at first
   for Character_Mapping_Data use record
      Reference_Count at 0 range
         0 ..
         System.Reference_Counting.Counter'Size - 1;
   end record;

   type Character_Mapping_Access is access all Character_Mapping_Data;
   for Character_Mapping_Access'Storage_Size use 0;

   function Value (Map : Character_Mapping_Data; Element : Character_Type)
      return Character_Type;

   --  for Handling

   function Translate (
      Source : String;
      Mapping : Character_Mapping_Data)
      return String;
   function Translate (
      Source : Wide_String;
      Mapping : Character_Mapping_Data)
      return Wide_String;
   function Translate (
      Source : Wide_Wide_String;
      Mapping : Character_Mapping_Data)
      return Wide_Wide_String;

   --  making operations

   procedure To_Mapping (
      From, To : Character_Sequence;
      Out_From, Out_To : out Character_Sequence; -- should have same 'First
      Out_Last : out Natural);

private

   type Character_Set_Array is
      array (Positive range <>) of not null access constant Character_Set_Data;
   pragma Suppress_Initialization (Character_Set_Array);

   --  for Set_Constants

   procedure Union (
      Target : out Character_Ranges;
      Last : out Natural;
      Source : in out Character_Set_Array); -- destructive

   --  for Case_Mapping

   procedure Sort (From, To : in out Character_Sequence);

end Ada.Strings.Naked_Maps;
