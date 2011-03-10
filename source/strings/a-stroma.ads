pragma License (Unrestricted);
--  extended package
private with Ada.Characters.Inside.Maps;
private with Ada.Characters.Inside.Sets;
private with Ada.Finalization;
private with Ada.Streams;
private with System.Reference_Counting;
package Ada.Strings.Root_Maps is
   pragma Preelaborate;

   type Character_Range is record
      Low : Character;
      High : Character;
   end record;
   type Character_Ranges is array (Positive range <>) of Character_Range;

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

   subtype Character_Sequence is String;
   subtype Wide_Character_Sequence is Wide_String;
   subtype Wide_Wide_Character_Sequence is Wide_Wide_String;

   type Root_Character_Set is private;
   pragma Preelaborable_Initialization (Root_Character_Set);

   function Null_Set return Root_Character_Set;
   pragma Inline (Null_Set);

   function Is_Null (Set : Root_Character_Set) return Boolean;
   pragma Inline (Is_Null);

   function Overloaded_To_Set (Ranges : Character_Ranges)
      return Root_Character_Set;
   function Overloaded_To_Set (Ranges : Wide_Character_Ranges)
      return Root_Character_Set;
   function Overloaded_To_Set (Ranges : Wide_Wide_Character_Ranges)
      return Root_Character_Set;

   function Overloaded_To_Set (Span : Character_Range)
      return Root_Character_Set;
   function Overloaded_To_Set (Span : Wide_Character_Range)
      return Root_Character_Set;
   function Overloaded_To_Set (Span : Wide_Wide_Character_Range)
      return Root_Character_Set;

   function Overloaded_To_Ranges (Set : Root_Character_Set)
      return Character_Ranges;
   function Overloaded_To_Ranges (Set : Root_Character_Set)
      return Wide_Character_Ranges;
   function Overloaded_To_Ranges (Set : Root_Character_Set)
      return Wide_Wide_Character_Ranges;

   function "=" (Left, Right : Root_Character_Set) return Boolean;

   function "not" (Right : Root_Character_Set)
      return Root_Character_Set;
   function "and" (Left, Right : Root_Character_Set)
      return Root_Character_Set;
   function "or" (Left, Right : Root_Character_Set)
      return Root_Character_Set;
   function "xor" (Left, Right : Root_Character_Set)
      return Root_Character_Set;
   function "-" (Left, Right : Root_Character_Set)
      return Root_Character_Set;

   function Overloaded_Is_In (
      Element : Character;
      Set : Root_Character_Set)
      return Boolean;
   function Overloaded_Is_In (
      Element : Wide_Character;
      Set : Root_Character_Set)
      return Boolean;
   function Overloaded_Is_In (
      Element : Wide_Wide_Character;
      Set : Root_Character_Set)
      return Boolean;

   function Is_Subset (
      Elements : Root_Character_Set;
      Set : Root_Character_Set)
      return Boolean;

   function Overloaded_To_Set (Sequence : Character_Sequence)
      return Root_Character_Set;
   function Overloaded_To_Set (Sequence : Wide_Character_Sequence)
      return Root_Character_Set;
   function Overloaded_To_Set (Sequence : Wide_Wide_Character_Sequence)
      return Root_Character_Set;

   function Overloaded_To_Set (Singleton : Character)
      return Root_Character_Set;
   function Overloaded_To_Set (Singleton : Wide_Character)
      return Root_Character_Set;
   function Overloaded_To_Set (Singleton : Wide_Wide_Character)
      return Root_Character_Set;

   function Overloaded_To_Sequence (Set : Root_Character_Set)
      return Character_Sequence;
   function Overloaded_To_Sequence (Set : Root_Character_Set)
      return Wide_Character_Sequence;
   function Overloaded_To_Sequence (Set : Root_Character_Set)
      return Wide_Wide_Character_Sequence;

   type Root_Character_Mapping is private;
   pragma Preelaborable_Initialization (Root_Character_Mapping);

   function Overloaded_Value (
      Map : Root_Character_Mapping;
      Element : Character)
      return Character;
   function Overloaded_Value (
      Map : Root_Character_Mapping;
      Element : Wide_Character)
      return Wide_Character;
   function Overloaded_Value (
      Map : Root_Character_Mapping;
      Element : Wide_Wide_Character)
      return Wide_Wide_Character;

   function Identity return Root_Character_Mapping;
   pragma Inline (Identity);

   function Is_Identity (Map : Root_Character_Mapping) return Boolean;
   pragma Inline (Is_Identity);

   function "=" (Left, Right : Root_Character_Mapping) return Boolean;

   function Overloaded_To_Mapping (From, To : Character_Sequence)
      return Root_Character_Mapping;
   function Overloaded_To_Mapping (From, To : Wide_Character_Sequence)
      return Root_Character_Mapping;
   function Overloaded_To_Mapping (From, To : Wide_Wide_Character_Sequence)
      return Root_Character_Mapping;

   function Overloaded_To_Domain (Map : Root_Character_Mapping)
      return Character_Sequence;
   function Overloaded_To_Domain (Map : Root_Character_Mapping)
      return Wide_Character_Sequence;
   function Overloaded_To_Domain (Map : Root_Character_Mapping)
      return Wide_Wide_Character_Sequence;

   function Overloaded_To_Range (Map : Root_Character_Mapping)
      return Character_Sequence;
   function Overloaded_To_Range (Map : Root_Character_Mapping)
      return Wide_Character_Sequence;
   function Overloaded_To_Range (Map : Root_Character_Mapping)
      return Wide_Wide_Character_Sequence;

private

   subtype Set_Data is Characters.Inside.Sets.Character_Set;
   type Set_Data_Access is access all Set_Data;

   Empty_Set_Data : constant Set_Data := (
      Length => 0,
      Reference_Count => System.Reference_Counting.Static,
      Items => <>);

   type Root_Character_Set is new Ada.Finalization.Controlled with record
      Data : aliased not null Set_Data_Access :=
         Empty_Set_Data'Unrestricted_Access;
   end record;

   overriding procedure Adjust (Object : in out Root_Character_Set);
   overriding procedure Finalize (Object : in out Root_Character_Set);

   package No_Primitives_For_Set is
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Root_Character_Set);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Root_Character_Set);
   end No_Primitives_For_Set;

   for Root_Character_Set'Read use No_Primitives_For_Set.Read;
   for Root_Character_Set'Write use No_Primitives_For_Set.Write;

   subtype Map_Data is Characters.Inside.Maps.Character_Mapping;
   type Map_Data_Access is access all Map_Data;

   Empty_Map_Data : constant Map_Data := (
      Length => 0,
      Reference_Count => System.Reference_Counting.Static,
      From => <>,
      To => <>);

   type Root_Character_Mapping is
      new Ada.Finalization.Controlled with
   record
      Data : aliased not null Map_Data_Access :=
         Empty_Map_Data'Unrestricted_Access;
   end record;

   overriding procedure Adjust (Object : in out Root_Character_Mapping);
   overriding procedure Finalize (Object : in out Root_Character_Mapping);

   package No_Primitives_For_Map is
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Root_Character_Mapping);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Root_Character_Mapping);
   end No_Primitives_For_Map;

   for Root_Character_Mapping'Read use No_Primitives_For_Map.Read;
   for Root_Character_Mapping'Write use No_Primitives_For_Map.Write;

end Ada.Strings.Root_Maps;
