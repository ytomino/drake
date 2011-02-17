pragma License (Unrestricted);
with Ada.Characters.Maps;
package Ada.Strings.Wide_Wide_Maps is
   pragma Preelaborate;

   --  Representation for a set of Wide_Wide_Character values:
--  type Wide_Wide_Character_Set is private;
   type Wide_Wide_Character_Set is new Characters.Maps.Root_Character_Set;
   pragma Preelaborable_Initialization (Wide_Wide_Character_Set);

--  Null_Set : constant Wide_Wide_Character_Set;
   --  function Null_Set is inherited

--  type Wide_Wide_Character_Range is record
--    Low : Wide_Wide_Character;
--    High : Wide_Wide_Character;
--  end record;
   subtype Wide_Wide_Character_Range is
      Characters.Maps.Wide_Wide_Character_Range;
   --  Represents Wide_Wide_Character range Low..High

--  type Wide_Wide_Character_Ranges is
--    array (Positive range <>) of Wide_Wide_Character_Range;
   subtype Wide_Wide_Character_Ranges is
      Characters.Maps.Wide_Wide_Character_Ranges;

   function To_Set (Ranges : Wide_Wide_Character_Ranges)
      return Wide_Wide_Character_Set
      renames Overloaded_To_Set;

   function To_Set (Span : Wide_Wide_Character_Range)
      return Wide_Wide_Character_Set
      renames Overloaded_To_Set;

   function To_Ranges (Set : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Ranges
      renames Overloaded_To_Ranges;

--  function "=" (Left, Right : Wide_Wide_Character_Set) return Boolean;
   --  function "=" is inherited

--  function "not" (Right : Wide_Wide_Character_Set)
--    return Wide_Wide_Character_Set;
--  function "and" (Left, Right : Wide_Wide_Character_Set)
--    return Wide_Wide_Character_Set;
--  function "or" (Left, Right : Wide_Wide_Character_Set)
--    return Wide_Wide_Character_Set;
--  function "xor" (Left, Right : Wide_Wide_Character_Set)
--    return Wide_Wide_Character_Set;
--  function "-" (Left, Right : Wide_Wide_Character_Set)
--    return Wide_Wide_Character_Set;
   --  "not", "and", "or", "xor" and "-" are inherited

   function Is_In (
      Element : Wide_Wide_Character;
      Set : Wide_Wide_Character_Set)
      return Boolean
      renames Overloaded_Is_In;

--  function Is_Subset (
--    Elements : Wide_Wide_Character_Set;
--    Set : Wide_Wide_Character_Set)
--    return Boolean;
   --  function Is_Subset is inherited

   function "<=" (
      Left : Wide_Wide_Character_Set;
      Right : Wide_Wide_Character_Set)
      return Boolean renames Is_Subset;

   --  Alternative representation for a set of Wide_Wide_Character values:
   subtype Wide_Wide_Character_Sequence is Wide_Wide_String;

   function To_Set (Sequence : Wide_Wide_Character_Sequence)
      return Wide_Wide_Character_Set
      renames Overloaded_To_Set;

   function To_Set (Singleton : Wide_Wide_Character)
      return Wide_Wide_Character_Set
      renames Overloaded_To_Set;

   function To_Sequence (Set : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Sequence
      renames Overloaded_To_Sequence;

   --  Representation for a Wide_Wide_Character to Wide_Wide_Character
   --  mapping:
--  type Wide_Wide_Character_Mapping is private;
   type Wide_Wide_Character_Mapping is
      new Characters.Maps.Root_Character_Mapping;
   pragma Preelaborable_Initialization (Wide_Wide_Character_Mapping);

   function Value (
      Map : Wide_Wide_Character_Mapping;
      Element : Wide_Wide_Character)
      return Wide_Wide_Character
      renames Overloaded_Value;

--  Identity : constant Wide_Wide_Character_Mapping;
   --  function Identity is inehrited

   function To_Mapping (From, To : Wide_Wide_Character_Sequence)
      return Wide_Wide_Character_Mapping
      renames Overloaded_To_Mapping;

   function To_Domain (Map : Wide_Wide_Character_Mapping)
      return Wide_Wide_Character_Sequence
      renames Overloaded_To_Domain;

   function To_Range (Map : Wide_Wide_Character_Mapping)
      return Wide_Wide_Character_Sequence
      renames Overloaded_To_Range;

   type Wide_Wide_Character_Mapping_Function is
      access function (From : Wide_Wide_Character) return Wide_Wide_Character;

end Ada.Strings.Wide_Wide_Maps;
