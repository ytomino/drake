pragma License (Unrestricted);
private with Ada.Finalization;
private with Ada.Streams;
private with Ada.Strings.Naked_Maps;
private with System.Reference_Counting;
package Ada.Strings.Maps is
   pragma Preelaborate;

   --  Representation for a set of character values:
   type Character_Set is private;
   pragma Preelaborable_Initialization (Character_Set);

   --  modified
--  Null_Set : constant Character_Set;
   function Null_Set return Character_Set;
   pragma Inline (Null_Set);

   --  extended
   function Is_Null (Set : Character_Set) return Boolean;
   pragma Inline (Is_Null);

   type Character_Range is record
      Low : Character;
      High : Character;
   end record;
      --  Represents Character range Low..High

   type Character_Ranges is array (Positive range <>) of Character_Range;

   --  extended from here

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

   overriding function "=" (Left, Right : Character_Set) return Boolean;

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

   --  modified
--  Identity : constant Character_Mapping;
   function Identity return Character_Mapping;
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

   --  extended
   overriding function "=" (Left, Right : Character_Mapping) return Boolean;

   type Character_Mapping_Function is
      access function (From : Character) return Character;

private

   subtype Set_Data is Naked_Maps.Character_Set_Data;
   type Set_Data_Access is access all Set_Data;

   Empty_Set_Data : aliased constant Set_Data := (
      Length => 0,
      Reference_Count => System.Reference_Counting.Static,
      Items => <>); -- [gcc-4.9/5] only this form does not generates _elabs (?)

   package Controlled_Sets is

      type Character_Set is private;

      function Create (Data : not null Set_Data_Access)
         return Character_Set;

      function Reference (Object : Maps.Character_Set)
         return not null Set_Data_Access;
      pragma Inline (Reference);

   private

      type Character_Set is new Finalization.Controlled with record
         Data : aliased not null Set_Data_Access :=
            Empty_Set_Data'Unrestricted_Access;
      end record;

      overriding procedure Adjust (Object : in out Character_Set);
      overriding procedure Finalize (Object : in out Character_Set);

      package Streaming is

         procedure Read (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : out Character_Set);
         procedure Write (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : Character_Set);

      end Streaming;

      for Character_Set'Read use Streaming.Read;
      for Character_Set'Write use Streaming.Write;

   end Controlled_Sets;

   type Character_Set is new Controlled_Sets.Character_Set;

   subtype Map_Data is Naked_Maps.Character_Mapping_Data;
   type Map_Data_Access is access all Map_Data;

   Empty_Map_Data : aliased constant Map_Data := (
      Length => 0,
      Reference_Count => System.Reference_Counting.Static,
      From => <>, -- [gcc-4.9/5] only this form does not generates _elabs (?)
      To => <>);

   package Controlled_Maps is

      type Character_Mapping is private;

      function Create (Data : not null Map_Data_Access)
         return Character_Mapping;

      function Reference (Object : Maps.Character_Mapping)
         return not null Map_Data_Access;
      pragma Inline (Reference);

   private

      type Character_Mapping is new Finalization.Controlled with record
         Data : aliased not null Map_Data_Access :=
            Empty_Map_Data'Unrestricted_Access;
      end record;

      overriding procedure Adjust (Object : in out Character_Mapping);
      overriding procedure Finalize (Object : in out Character_Mapping);

      package Streaming is

         procedure Read (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : out Character_Mapping);
         procedure Write (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : Character_Mapping);

      end Streaming;

      for Character_Mapping'Read use Streaming.Read;
      for Character_Mapping'Write use Streaming.Write;

   end Controlled_Maps;

   type Character_Mapping is new Controlled_Maps.Character_Mapping;

end Ada.Strings.Maps;
