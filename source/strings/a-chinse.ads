pragma License (Unrestricted);
--  implementation unit
with System.Reference_Counting;
package Ada.Characters.Inside.Sets is
   pragma Pure;

   subtype Character_Type is Wide_Wide_Character;
   subtype Character_Sequence is Wide_Wide_String;

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
   function Is_In (
      Element : Character;
      Set : Character_Set)
      return Boolean;

   --  binary tree operations

   procedure Add (
      A : in out Character_Ranges;
      Last : in out Natural;
      L, H : Character_Type);

   procedure Merge (
      Target : out Character_Ranges;
      Last : out Natural;
      Left, Right : Character_Ranges);

   type Character_Ranges_Array is
      array (Positive range <>) of not null access constant Character_Ranges;
   pragma Suppress_Initialization (Character_Ranges_Array);

   procedure Merge (
      Target : out Character_Ranges;
      Last : out Natural;
      Source : Character_Ranges_Array);

end Ada.Characters.Inside.Sets;
