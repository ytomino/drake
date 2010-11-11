pragma License (Unrestricted);
private with Ada.Strings.Wide_Wide_Maps;
package Ada.Strings.Maps is
--  pragma Pure;
   pragma Preelaborate; --  Wide_Wide_Maps is "preelaborate"

   --  Representation for a set of character values:
   type Character_Set is private;
   pragma Preelaborable_Initialization (Character_Set);

--  Null_Set : constant Character_Set;
   function Null_Set return Character_Set; --  extended
   pragma Inline (Null_Set);

   type Character_Range is record
      Low : Character;
      High : Character;
   end record;
   --  Represents Character range Low..High

   type Character_Ranges is array (Positive range <>) of Character_Range;

   function To_Set (Ranges : Character_Ranges) return Character_Set;

   function To_Set (Span : Character_Range) return Character_Set;

--  function To_Ranges (Set : Character_Set) return Character_Ranges;

   function "=" (Left, Right : Character_Set) return Boolean;
   pragma Inline ("=");

   function "not" (Right : Character_Set) return Character_Set;
   pragma Inline ("not");
   function "and" (Left, Right : Character_Set) return Character_Set;
   pragma Inline ("and");
   function "or" (Left, Right : Character_Set) return Character_Set;
   pragma Inline ("or");
   function "xor" (Left, Right : Character_Set) return Character_Set;
   pragma Inline ("xor");
   function "-" (Left, Right : Character_Set) return Character_Set;
   pragma Inline ("-");

   function Is_In (Element : Character; Set : Character_Set)
      return Boolean;
   pragma Inline (Is_In);

   function Is_Subset (Elements : Character_Set; Set : Character_Set)
      return Boolean;
   pragma Inline (Is_Subset);

   function "<=" (Left : Character_Set; Right : Character_Set)
      return Boolean renames Is_Subset;

   --  Alternative representation for a set of character values:
   subtype Character_Sequence is String;

   function To_Set (Sequence : Character_Sequence) return Character_Set;

   function To_Set (Singleton : Character) return Character_Set;

   function To_Sequence (Set : Character_Set) return Character_Sequence;

   --  Representation for a character to character mapping:
   type Character_Mapping is private;
   pragma Preelaborable_Initialization (Character_Mapping);

   function Value (Map : Character_Mapping; Element : Character)
      return Character;

--  Identity : constant Character_Mapping;
   function Identity return Character_Mapping; --  extended
   pragma Inline (Identity);

   function To_Mapping (From, To : Character_Sequence)
      return Character_Mapping;

   function To_Domain (Map : Character_Mapping)
      return Character_Sequence;
   function To_Range (Map : Character_Mapping)
      return Character_Sequence;

   type Character_Mapping_Function is
      access function (From : Character) return Character;

private

   type Character_Set is new Wide_Wide_Maps.Wide_Wide_Character_Set;
   type Character_Mapping is new Wide_Wide_Maps.Wide_Wide_Character_Mapping;

end Ada.Strings.Maps;
