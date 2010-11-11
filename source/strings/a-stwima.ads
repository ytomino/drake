pragma License (Unrestricted);
private with Ada.Strings.Wide_Wide_Maps;
package Ada.Strings.Wide_Maps is
   pragma Preelaborate;

   --  Representation for a set of Wide_Character values:
   type Wide_Character_Set is private;
   pragma Preelaborable_Initialization (Wide_Character_Set);

--  Null_Set : constant Wide_Character_Set;
   function Null_Set return Wide_Character_Set; --  extended
   pragma Inline (Null_Set);

   type Wide_Character_Range is record
      Low : Wide_Character;
      High : Wide_Character;
   end record;
   --  Represents Wide_Character range Low..High

   type Wide_Character_Ranges is
      array (Positive range <>) of Wide_Character_Range;

   function To_Set (Ranges : Wide_Character_Ranges)
      return Wide_Character_Set;

   function To_Set (Span : Wide_Character_Range)
      return Wide_Character_Set;

--  function To_Ranges (Set : Wide_Character_Set) return Wide_Character_Ranges;

   function "=" (Left, Right : Wide_Character_Set) return Boolean;
   pragma Inline ("=");

   function "not" (Right : Wide_Character_Set) return Wide_Character_Set;
   pragma Inline ("not");
   function "and" (Left, Right : Wide_Character_Set) return Wide_Character_Set;
   pragma Inline ("and");
   function "or" (Left, Right : Wide_Character_Set) return Wide_Character_Set;
   pragma Inline ("or");
   function "xor" (Left, Right : Wide_Character_Set) return Wide_Character_Set;
   pragma Inline ("xor");
   function "-" (Left, Right : Wide_Character_Set) return Wide_Character_Set;
   pragma Inline ("-");

   function Is_In (Element : Wide_Character; Set : Wide_Character_Set)
      return Boolean;
   pragma Inline (Is_In);

   function Is_Subset (Elements : Wide_Character_Set; Set : Wide_Character_Set)
      return Boolean;
   pragma Inline (Is_Subset);

   function "<=" (Left : Wide_Character_Set; Right : Wide_Character_Set)
      return Boolean renames Is_Subset;

   --  Alternative representation for a set of Wide_Character values:
   subtype Wide_Character_Sequence is Wide_String;

   function To_Set (Sequence : Wide_Character_Sequence)
      return Wide_Character_Set;

   function To_Set (Singleton : Wide_Character)
      return Wide_Character_Set;

   function To_Sequence (Set : Wide_Character_Set)
      return Wide_Character_Sequence;

   --  Representation for a Wide_Character to Wide_Character mapping:
   type Wide_Character_Mapping is private;
   pragma Preelaborable_Initialization (Wide_Character_Mapping);

   function Value (Map : Wide_Character_Mapping; Element : Wide_Character)
      return Wide_Character;

--  Identity : constant Wide_Character_Mapping;
   function Identity return Wide_Character_Mapping; --  extended
   pragma Inline (Identity);

   function To_Mapping (From, To : Wide_Character_Sequence)
      return Wide_Character_Mapping;

   function To_Domain (Map : Wide_Character_Mapping)
      return Wide_Character_Sequence;

   function To_Range (Map : Wide_Character_Mapping)
      return Wide_Character_Sequence;

   type Wide_Character_Mapping_Function is
      access function (From : Wide_Character) return Wide_Character;

private

   type Wide_Character_Set is new Wide_Wide_Maps.Wide_Wide_Character_Set;
   type Wide_Character_Mapping is
      new Wide_Wide_Maps.Wide_Wide_Character_Mapping;

end Ada.Strings.Wide_Maps;
