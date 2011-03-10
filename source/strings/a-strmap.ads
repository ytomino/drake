pragma License (Unrestricted);
with Ada.Strings.Root_Maps;
package Ada.Strings.Maps is
--  pragma Pure;
   pragma Preelaborate; -- controlled types

   --  Representation for a set of character values:
--  type Character_Set is private;
   type Character_Set is new Root_Maps.Root_Character_Set;
   pragma Preelaborable_Initialization (Character_Set);

--  Null_Set : constant Character_Set;
   --  function Null_Set is inherited

--  type Character_Range is record
--    Low : Character;
--    High : Character;
--  end record;
   subtype Character_Range is Root_Maps.Character_Range;
   --  Represents Character range Low..High

--  type Character_Ranges is array (Positive range <>) of Character_Range;
   subtype Character_Ranges is Root_Maps.Character_Ranges;

   function To_Set (Ranges : Character_Ranges) return Character_Set
      renames Overloaded_To_Set;

   function To_Set (Span : Character_Range) return Character_Set
      renames Overloaded_To_Set;

   function To_Ranges (Set : Character_Set) return Character_Ranges
      renames Overloaded_To_Ranges;

--  function "=" (Left, Right : Character_Set) return Boolean;
   --  function "=" is inherited

--  function "not" (Right : Character_Set) return Character_Set;
--  function "and" (Left, Right : Character_Set) return Character_Set;
--  function "or" (Left, Right : Character_Set) return Character_Set;
--  function "xor" (Left, Right : Character_Set) return Character_Set;
--  function "-" (Left, Right : Character_Set) return Character_Set;
   --  "not", "and", "or", "xor" and "-" are inherited

   function Is_In (Element : Character; Set : Character_Set)
      return Boolean
      renames Overloaded_Is_In;

--  function Is_Subset (Elements : Character_Set; Set : Character_Set)
--    return Boolean;
   --  function Is_Subset is inherited

   function "<=" (Left : Character_Set; Right : Character_Set)
      return Boolean renames Is_Subset;

   --  Alternative representation for a set of character values:
   subtype Character_Sequence is String;

   function To_Set (Sequence : Character_Sequence) return Character_Set
      renames Overloaded_To_Set;

   function To_Set (Singleton : Character) return Character_Set
      renames Overloaded_To_Set;

   function To_Sequence (Set : Character_Set) return Character_Sequence
      renames Overloaded_To_Sequence;

   --  Representation for a character to character mapping:
--  type Character_Mapping is private;
   type Character_Mapping is new Root_Maps.Root_Character_Mapping;
   pragma Preelaborable_Initialization (Character_Mapping);

   function Value (Map : Character_Mapping; Element : Character)
      return Character
      renames Overloaded_Value;

--  Identity : constant Character_Mapping;
   --  function Identity is inehrited

   function To_Mapping (From, To : Character_Sequence)
      return Character_Mapping
      renames Overloaded_To_Mapping;

   function To_Domain (Map : Character_Mapping)
      return Character_Sequence
      renames Overloaded_To_Domain;
   function To_Range (Map : Character_Mapping)
      return Character_Sequence
      renames Overloaded_To_Range;

   type Character_Mapping_Function is
      access function (From : Character) return Character;

end Ada.Strings.Maps;
