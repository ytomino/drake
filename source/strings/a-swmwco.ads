pragma License (Unrestricted);
with Ada.Strings.Maps.Constants;
--  diff (Ada.Strings.Maps.Naked)
--  diff (Ada.Strings.Naked_Maps.Set_Constants)
package Ada.Strings.Wide_Maps.Wide_Constants is
   pragma Preelaborate;

   --  extended
   --  There are sets of unicode category.
   function Unassigned_Set return Wide_Character_Set
      renames Maps.Constants.Unassigned_Set;
   function Uppercase_Letter_Set return Wide_Character_Set
      renames Maps.Constants.Uppercase_Letter_Set;
   function Lowercase_Letter_Set return Wide_Character_Set
      renames Maps.Constants.Lowercase_Letter_Set;
   function Titlecase_Letter_Set return Wide_Character_Set
      renames Maps.Constants.Titlecase_Letter_Set;
   function Modifier_Letter_Set return Wide_Character_Set
      renames Maps.Constants.Modifier_Letter_Set;
   function Other_Letter_Set return Wide_Character_Set
      renames Maps.Constants.Other_Letter_Set;
   function Decimal_Number_Set return Wide_Character_Set
      renames Maps.Constants.Decimal_Number_Set;
   function Letter_Number_Set return Wide_Character_Set
      renames Maps.Constants.Letter_Number_Set;
   function Other_Number_Set return Wide_Character_Set
      renames Maps.Constants.Other_Number_Set;
   function Line_Separator_Set return Wide_Character_Set
      renames Maps.Constants.Line_Separator_Set;
   function Paragraph_Separator_Set return Wide_Character_Set
      renames Maps.Constants.Paragraph_Separator_Set;
   function Control_Set return Wide_Character_Set
      renames Maps.Constants.Control_Set;
   function Format_Set return Wide_Character_Set
      renames Maps.Constants.Format_Set;
   function Private_Use_Set return Wide_Character_Set
      renames Maps.Constants.Private_Use_Set;
   function Surrogate_Set return Wide_Character_Set
      renames Maps.Constants.Surrogate_Set;

   --  extended
   function Base_Set return Wide_Character_Set
      renames Maps.Constants.Base_Set;

--  Control_Set : constant Wide_Character_Set;
      --  Control_Set is declared as unicode category in above.
--  Graphic_Set : constant Wide_Character_Set;
   function Graphic_Set return Wide_Character_Set
      renames Maps.Constants.Graphic_Set;
--  Letter_Set : constant Wide_Character_Set;
   function Letter_Set return Wide_Character_Set
      renames Maps.Constants.Letter_Set;
--  Lower_Set : constant Wide_Character_Set;
   function Lower_Set return Wide_Character_Set
      renames Lowercase_Letter_Set;
      --  Note: Lower_Set is extended for all unicode characters.
--  Upper_Set : constant Wide_Character_Set;
   function Upper_Set return Wide_Character_Set
      renames Uppercase_Letter_Set;
      --  Note: Upper_Set is extended for all unicode characters.
--  Basic_Set : constant Wide_Character_Set;
   function Basic_Set return Wide_Character_Set
      renames Maps.Constants.Basic_Set;
      --  Note: Basic_Set is extended for all unicode characters.
--  Decimal_Digit_Set : constant Wide_Character_Set;
   function Decimal_Digit_Set return Wide_Character_Set
      renames Maps.Constants.Decimal_Digit_Set;
      --  Note: Decimal_Digit_Set is NOT extended for parsing.
--  Hexadecimal_Digit_Set : constant Wide_Character_Set;
   function Hexadecimal_Digit_Set return Wide_Character_Set
      renames Maps.Constants.Hexadecimal_Digit_Set;
      --  Note: Hexadecimal_Digit_Set is NOT extended for parsing.
--  Alphanumeric_Set : constant Wide_Character_Set;
   function Alphanumeric_Set return Wide_Character_Set
      renames Maps.Constants.Alphanumeric_Set;
--  Special_Set : constant Wide_Character_Set;
   function Special_Set return Wide_Character_Set
      renames Maps.Constants.Special_Set;
--  ISO_646_Set : constant Wide_Character_Set;
   function ISO_646_Set return Wide_Character_Set
      renames Maps.Constants.ISO_646_Set;

--  Lower_Case_Map : constant Wide_Character_Mapping;
   function Lower_Case_Map return Wide_Character_Mapping
      renames Maps.Constants.Lower_Case_Map;
      --  Maps to lower case for letters, else identity
      --  Note: Lower_Case_Map is extended for all unicode characters.
--  Upper_Case_Map : constant Wide_Character_Mapping;
   function Upper_Case_Map return Wide_Character_Mapping
      renames Maps.Constants.Upper_Case_Map;
      --  Maps to upper case for letters, else identity
      --  Note: Upper_Case_Map is extended for all unicode characters.
   --  extended from here
   function Case_Folding_Map return Wide_Character_Mapping
      renames Maps.Constants.Case_Folding_Map;
   function Base_Map return Wide_Character_Mapping
      renames Maps.Constants.Base_Map;
   --  to here
--  Basic_Map : constant Wide_Character_Mapping;
   function Basic_Map return Wide_Character_Mapping
      renames Maps.Constants.Basic_Map;
      --  Maps to basic letter for letters, else identity
      --  Note: Basic_Map is extended for all unicode characters.

   --  RM A.4.7

--  Character_Set : constant Wide_Maps.Wide_Character_Set;
   function Character_Set return Wide_Character_Set
      renames ISO_646_Set;
      --  Contains each Wide_Character value WC such that
      --  Characters.Conversions.Is_Character(WC) is True
      --  Note: (16#7F# .. 16#FF#) is excluded from Character_Set.

end Ada.Strings.Wide_Maps.Wide_Constants;
