pragma License (Unrestricted);
with Ada.Characters.Inside.Maps;
with Ada.Characters.Inside.Maps.Case_Folding;
with Ada.Characters.Inside.Maps.Lower_Case;
with Ada.Characters.Inside.Maps.Upper_Case;
with Ada.Characters.Inside.Sets;
with Ada.Characters.Inside.Sets.Constants;
with Ada.Characters.Inside.Sets.General_Category;
with Ada.Characters.Maps.Inside;
package Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants is
   pragma Preelaborate;

   --  extended sets of unicode category
   function Unassigned_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.All_Unassigned);
   function Uppercase_Letter_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Uppercase_Letter);
   function Lowercase_Letter_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Lowercase_Letter);
   function Titlecase_Letter_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Titlecase_Letter);
   function Modifier_Letter_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Modifier_Letter);
   function Other_Letter_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Other_Letter);
   function Decimal_Number_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Decimal_Number);
   function Letter_Number_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Letter_Number);
   function Other_Number_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Other_Number);
   function Line_Separator_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Line_Separator);
   function Paragraph_Separator_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Paragraph_Separator);
   function Control_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Control);
   function Format_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Format);
   function Private_Use_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Private_Use);
   function Surrogate_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Surrogate);

--  Control_Set : constant Wide_Wide_Character_Set;
   --  (Control_Set is declared as unicode category in above)
--  Graphic_Set : constant Wide_Wide_Character_Set;
   function Graphic_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Graphic_Set);
--  Letter_Set : constant Wide_Wide_Character_Set;
   function Letter_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Letter_Set);
--  Lower_Set : constant Wide_Wide_Character_Set;
   function Lower_Set return Wide_Wide_Character_Set
      renames Lowercase_Letter_Set;
   --  (Lower_Set is extended for all unicode characters)
--  Upper_Set : constant Wide_Wide_Character_Set;
   function Upper_Set return Wide_Wide_Character_Set
      renames Uppercase_Letter_Set;
   --  (Upper_Set is extended for all unicode characters)
--  Basic_Set : constant Wide_Wide_Character_Set;
   function Decimal_Digit_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Decimal_Digit_Set);
   function Hexadecimal_Digit_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Hexadecimal_Digit_Set);
   --  (Decimal_Digit_Set, Hexadecimal_Digit_Set are NOT extended, for parsing)
--  Alphanumeric_Set : constant Wide_Wide_Character_Set;
   function Alphanumeric_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Alphanumeric_Set);
--  Special_Set : constant Wide_Wide_Character_Set;
   function Special_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Special_Set);
--  ISO_646_Set : constant Wide_Wide_Character_Set;
   function ISO_646_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.ISO_646_Set);

--  Lower_Case_Map : constant Wide_Wide_Character_Mapping;
   function Lower_Case_Map is new Characters.Maps.Inside.To_Mapping (
      Wide_Wide_Character_Mapping,
      Characters.Inside.Maps.Lower_Case.Lower_Case_Map);
   --  Maps to lower case for letters, else identity
   --  (Lower_Case_Map is extended for all unicode characters)
--  Upper_Case_Map : constant Wide_Wide_Character_Mapping;
   function Upper_Case_Map is new Characters.Maps.Inside.To_Mapping (
      Wide_Wide_Character_Mapping,
      Characters.Inside.Maps.Upper_Case.Upper_Case_Map);
   --  Maps to upper case for letters, else identity
   --  (Upper_Case_Map is extended for all unicode characters)
--  Basic_Map : constant Wide_Wide_Character_Mapping;
   --  Maps to basic letter for letters, else identity

   --  extended
   function Case_Folding_Map is new Characters.Maps.Inside.To_Mapping (
      Wide_Wide_Character_Mapping,
      Characters.Inside.Maps.Case_Folding.Case_Folding_Map);

   --  RM A.4.8

--  Character_Set : constant Wide_Wide_Maps.Wide_Wide_Character_Set;
   --  Contains each Wide_Wide_Character value WWC such that
   --  Characters.Conversions.Is_Character(WWC) is True
   function Character_Set return Wide_Wide_Character_Set
      renames ISO_646_Set;
   --  (Character_Set is excluded 16#7F# .. 16#FF#)
--  Wide_Character_Set : constant Wide_Wide_Maps.Wide_Wide_Character_Set;
   --  Contains each Wide_Wide_Character value WWC such that
   --  Characters.Conversions.Is_Wide_Character(WWC) is True
   function Wide_Character_Set is new Characters.Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Wide_Character_Set);
   --  (Wide_Character_Set is excluded surrogate pair)

end Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
