pragma License (Unrestricted);
private with Ada.Characters.Inside.Maps;
private with Ada.Characters.Inside.Sets;
private with Ada.Finalization;
private with Ada.Streams;
private with System.Reference_Counting;
package Ada.Strings.Maps is
   pragma Preelaborate;

   --  Representation for a set of character values:
   type Character_Set is private;
   pragma Preelaborable_Initialization (Character_Set);

--  Null_Set : constant Character_Set;
   function Null_Set return Character_Set; -- extended
   pragma Inline (Null_Set);

   --  extended
   function Is_Null (Set : Character_Set) return Boolean;
   pragma Inline (Is_Null);

   type Character_Range is record
      Low  : Character;
      High : Character;
   end record;
   --  Represents Character range Low..High

   type Character_Ranges is array (Positive range <>) of Character_Range;

   --  extended
   type Wide_Character_Range is record
      Low : Wide_Character;
      High : Wide_Character;
   end record;
   type Wide_Character_Ranges is
      array (Positive range <>) of Wide_Character_Range;
   type Wide_Wide_Character_Range is record
      Low : Wide_Wide_Character;
      High : Wide_Wide_Character;
   end record;
   type Wide_Wide_Character_Ranges is
      array (Positive range <>) of Wide_Wide_Character_Range;

   --  extended
   function Overloaded_To_Set (Ranges : Character_Ranges)
      return Character_Set;
   function Overloaded_To_Set (Ranges : Wide_Character_Ranges)
      return Character_Set;
   function Overloaded_To_Set (Ranges : Wide_Wide_Character_Ranges)
      return Character_Set;

   function To_Set (Ranges : Character_Ranges) return Character_Set
      renames Overloaded_To_Set;

   --  extended
   function Overloaded_To_Set (Span : Character_Range)
      return Character_Set;
   function Overloaded_To_Set (Span : Wide_Character_Range)
      return Character_Set;
   function Overloaded_To_Set (Span : Wide_Wide_Character_Range)
      return Character_Set;

   function To_Set (Span : Character_Range) return Character_Set
      renames Overloaded_To_Set;

   --  extended
   function Overloaded_To_Ranges (Set : Character_Set)
      return Character_Ranges;
   function Overloaded_To_Ranges (Set : Character_Set)
      return Wide_Character_Ranges;
   function Overloaded_To_Ranges (Set : Character_Set)
      return Wide_Wide_Character_Ranges;

   function To_Ranges (Set : Character_Set) return Character_Ranges
      renames Overloaded_To_Ranges;

   function "=" (Left, Right : Character_Set) return Boolean;

   function "not" (Right : Character_Set) return Character_Set;
   function "and" (Left, Right : Character_Set) return Character_Set;
   function "or" (Left, Right : Character_Set) return Character_Set;
   function "xor" (Left, Right : Character_Set) return Character_Set;
   function "-" (Left, Right : Character_Set) return Character_Set;

   --  extended
   function Overloaded_Is_In (
      Element : Character;
      Set : Character_Set)
      return Boolean;
   function Overloaded_Is_In (
      Element : Wide_Character;
      Set : Character_Set)
      return Boolean;
   function Overloaded_Is_In (
      Element : Wide_Wide_Character;
      Set : Character_Set)
      return Boolean;

   function Is_In (Element : Character; Set : Character_Set) return Boolean
      renames Overloaded_Is_In;

   function Is_Subset (Elements : Character_Set; Set : Character_Set)
      return Boolean;

   function "<=" (Left : Character_Set; Right : Character_Set) return Boolean
      renames Is_Subset;

   --  Alternative representation for a set of character values:
   subtype Character_Sequence is String;

   --  extended
   subtype Wide_Character_Sequence is Wide_String;
   subtype Wide_Wide_Character_Sequence is Wide_Wide_String;

   --  extended
   function Overloaded_To_Set (Sequence : Character_Sequence)
      return Character_Set;
   function Overloaded_To_Set (Sequence : Wide_Character_Sequence)
      return Character_Set;
   function Overloaded_To_Set (Sequence : Wide_Wide_Character_Sequence)
      return Character_Set;

   function To_Set (Sequence : Character_Sequence) return Character_Set
      renames Overloaded_To_Set;

   --  extended
   function Overloaded_To_Set (Singleton : Character)
      return Character_Set;
   function Overloaded_To_Set (Singleton : Wide_Character)
      return Character_Set;
   function Overloaded_To_Set (Singleton : Wide_Wide_Character)
      return Character_Set;

   function To_Set (Singleton : Character) return Character_Set
      renames Overloaded_To_Set;

   --  extended
   function Overloaded_To_Sequence (Set : Character_Set)
      return Character_Sequence;
   function Overloaded_To_Sequence (Set : Character_Set)
      return Wide_Character_Sequence;
   function Overloaded_To_Sequence (Set : Character_Set)
      return Wide_Wide_Character_Sequence;

   function To_Sequence (Set : Character_Set) return Character_Sequence
      renames Overloaded_To_Sequence;

   --  Representation for a character to character mapping:
   type Character_Mapping is private;
   pragma Preelaborable_Initialization (Character_Mapping);

   --  extended
   function Overloaded_Value (
      Map : Character_Mapping;
      Element : Character)
      return Character;
   function Overloaded_Value (
      Map : Character_Mapping;
      Element : Wide_Character)
      return Wide_Character;
   function Overloaded_Value (
      Map : Character_Mapping;
      Element : Wide_Wide_Character)
      return Wide_Wide_Character;

   function Value (Map : Character_Mapping; Element : Character)
      return Character
      renames Overloaded_Value;

--  Identity : constant Character_Mapping;
   function Identity return Character_Mapping; -- extended
   pragma Inline (Identity);

   --  extended
   function Is_Identity (Map : Character_Mapping) return Boolean;
   pragma Inline (Is_Identity);

   --  extended
   function Overloaded_To_Mapping (From, To : Character_Sequence)
      return Character_Mapping;
   function Overloaded_To_Mapping (From, To : Wide_Character_Sequence)
      return Character_Mapping;
   function Overloaded_To_Mapping (From, To : Wide_Wide_Character_Sequence)
      return Character_Mapping;

   function To_Mapping (From, To : Character_Sequence)
      return Character_Mapping
      renames Overloaded_To_Mapping;

   --  extended
   function "=" (Left, Right : Character_Mapping) return Boolean;

   --  extended
   function Overloaded_To_Domain (Map : Character_Mapping)
      return Character_Sequence;
   function Overloaded_To_Domain (Map : Character_Mapping)
      return Wide_Character_Sequence;
   function Overloaded_To_Domain (Map : Character_Mapping)
      return Wide_Wide_Character_Sequence;
   function Overloaded_To_Range (Map : Character_Mapping)
      return Character_Sequence;
   function Overloaded_To_Range (Map : Character_Mapping)
      return Wide_Character_Sequence;
   function Overloaded_To_Range (Map : Character_Mapping)
      return Wide_Wide_Character_Sequence;

   function To_Domain (Map : Character_Mapping)
      return Character_Sequence
      renames Overloaded_To_Domain;
   function To_Range (Map : Character_Mapping)
      return Character_Sequence
      renames Overloaded_To_Range;

   type Character_Mapping_Function is
      access function (From : Character) return Character;

private

   subtype Set_Data is Characters.Inside.Sets.Character_Set;
   type Set_Data_Access is access all Set_Data;

   Empty_Set_Data : constant Set_Data := (
      Length => 0,
      Reference_Count => System.Reference_Counting.Static,
      Items => <>);

   type Character_Set is new Finalization.Controlled with record
      Data : aliased not null Set_Data_Access :=
         Empty_Set_Data'Unrestricted_Access;
   end record;

   overriding procedure Adjust (Object : in out Character_Set);
   overriding procedure Finalize (Object : in out Character_Set);

   package No_Primitives_For_Set is
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Character_Set);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Character_Set);
   end No_Primitives_For_Set;

   for Character_Set'Read use No_Primitives_For_Set.Read;
   for Character_Set'Write use No_Primitives_For_Set.Write;

   subtype Map_Data is Characters.Inside.Maps.Character_Mapping;
   type Map_Data_Access is access all Map_Data;

   Empty_Map_Data : constant Map_Data := (
      Length => 0,
      Reference_Count => System.Reference_Counting.Static,
      From => <>,
      To => <>);

   type Character_Mapping is new Finalization.Controlled with record
      Data : aliased not null Map_Data_Access :=
         Empty_Map_Data'Unrestricted_Access;
   end record;

   overriding procedure Adjust (Object : in out Character_Mapping);
   overriding procedure Finalize (Object : in out Character_Mapping);

   package No_Primitives_For_Map is
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Character_Mapping);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Character_Mapping);
   end No_Primitives_For_Map;

   for Character_Mapping'Read use No_Primitives_For_Map.Read;
   for Character_Mapping'Write use No_Primitives_For_Map.Write;

end Ada.Strings.Maps;
