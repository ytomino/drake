pragma License (Unrestricted);
--  implementation package
with Interfaces;
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
      Reference_Count : aliased Interfaces.Integer_32; --  -1 as constant
      Items : Character_Ranges (1 .. Length);
   end record;
   pragma Suppress_Initialization (Character_Set);

   function Is_In (Element : Character_Type; Set : Character_Set)
      return Boolean;

   --  binary tree operations
   procedure Add (
      A : in out Character_Ranges;
      Last : in out Natural;
      L, H : Character_Type);
   function Search (A : Character_Ranges; L, H : Character_Type)
      return Positive;

end Ada.Characters.Inside.Sets;
