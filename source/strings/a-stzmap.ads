pragma License (Unrestricted);
private with Ada.Characters.Inside;
private with Ada.Finalization;
private with Interfaces;
package Ada.Strings.Wide_Wide_Maps is
   pragma Preelaborate;

   --  Representation for a set of Wide_Wide_Character values:
   type Wide_Wide_Character_Set is private;
   pragma Preelaborable_Initialization (Wide_Wide_Character_Set);

--  Null_Set : constant Wide_Wide_Character_Set;
   function Null_Set return Wide_Wide_Character_Set;

   type Wide_Wide_Character_Range is record
      Low : Wide_Wide_Character;
      High : Wide_Wide_Character;
   end record;
   --  Represents Wide_Wide_Character range Low..High

   type Wide_Wide_Character_Ranges is
      array (Positive range <>) of Wide_Wide_Character_Range;

   function To_Set (Ranges : Wide_Wide_Character_Ranges)
      return Wide_Wide_Character_Set;

   function To_Set (Span : Wide_Wide_Character_Range)
      return Wide_Wide_Character_Set;

   function To_Ranges (Set : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Ranges;

   function "=" (Left, Right : Wide_Wide_Character_Set) return Boolean;

   function "not" (Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set;
   function "and" (Left, Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set;
   function "or" (Left, Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set;
   function "xor" (Left, Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set;
   function "-" (Left, Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set;

   function Is_In (
      Element : Wide_Wide_Character;
      Set : Wide_Wide_Character_Set)
      return Boolean;

   function Is_Subset (
      Elements : Wide_Wide_Character_Set;
      Set : Wide_Wide_Character_Set)
      return Boolean;

   function "<=" (
      Left : Wide_Wide_Character_Set;
      Right : Wide_Wide_Character_Set)
      return Boolean renames Is_Subset;

   --  Alternative representation for a set of Wide_Wide_Character values:
   subtype Wide_Wide_Character_Sequence is Wide_Wide_String;

   function To_Set (Sequence : Wide_Wide_Character_Sequence)
      return Wide_Wide_Character_Set;

   function To_Set (Singleton : Wide_Wide_Character)
      return Wide_Wide_Character_Set;

   function To_Sequence (Set : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Sequence;

   --  Representation for a Wide_Wide_Character to Wide_Wide_Character
   --  mapping:
   type Wide_Wide_Character_Mapping is private;
   pragma Preelaborable_Initialization (Wide_Wide_Character_Mapping);

   function Value (
      Map : Wide_Wide_Character_Mapping;
      Element : Wide_Wide_Character)
      return Wide_Wide_Character;

--  Identity : constant Wide_Wide_Character_Mapping;
   function Identity return Wide_Wide_Character_Mapping;

   function To_Mapping (From, To : Wide_Wide_Character_Sequence)
      return Wide_Wide_Character_Mapping;

   function To_Domain (Map : Wide_Wide_Character_Mapping)
      return Wide_Wide_Character_Sequence;

   function To_Range (Map : Wide_Wide_Character_Mapping)
      return Wide_Wide_Character_Sequence;

   type Wide_Wide_Character_Mapping_Function is
      access function (From : Wide_Wide_Character) return Wide_Wide_Character;

private

   type Set_Data (Length : Natural) is record
      Reference_Count : aliased Interfaces.Integer_32; --  -1 as constant
      Items : Wide_Wide_Character_Ranges (1 .. Length);
   end record;
   pragma Suppress_Initialization (Set_Data);

   type Set_Data_Access is access all Set_Data;

   type Wide_Wide_Character_Set is
      new Ada.Finalization.Controlled with
   record
      Data : Set_Data_Access;
   end record;

   overriding procedure Adjust (Object : in out Wide_Wide_Character_Set);
   overriding procedure Finalize (Object : in out Wide_Wide_Character_Set);

   subtype Map_Data is Characters.Inside.Character_Mapping;
   type Map_Data_Access is access all Map_Data;

   type Wide_Wide_Character_Mapping is
      new Ada.Finalization.Controlled with
   record
      Data : Map_Data_Access;
   end record;

   overriding procedure Adjust (Object : in out Wide_Wide_Character_Mapping);
   overriding procedure Finalize (Object : in out Wide_Wide_Character_Mapping);

end Ada.Strings.Wide_Wide_Maps;
