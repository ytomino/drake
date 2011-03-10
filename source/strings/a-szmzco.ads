pragma License (Unrestricted);
private with Ada.Characters.Inside.Maps;
private with Ada.Characters.Inside.Maps.Case_Folding;
private with Ada.Characters.Inside.Maps.Lower_Case;
private with Ada.Characters.Inside.Maps.Upper_Case;
private with Ada.Characters.Inside.Sets;
private with Ada.Characters.Inside.Sets.Constants;
private with Ada.Characters.Inside.Sets.General_Category;
private with Ada.Strings.Root_Maps.Inside;
package Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants is
   pragma Preelaborate;

   --  extended sets of unicode category
   function Unassigned_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Unassigned_Set);
   function Uppercase_Letter_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Uppercase_Letter_Set);
   function Lowercase_Letter_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Lowercase_Letter_Set);
   function Titlecase_Letter_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Titlecase_Letter_Set);
   function Modifier_Letter_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Modifier_Letter_Set);
   function Other_Letter_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Other_Letter_Set);
   function Decimal_Number_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Decimal_Number_Set);
   function Letter_Number_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Letter_Number_Set);
   function Other_Number_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Other_Number_Set);
   function Line_Separator_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Line_Separator_Set);
   function Paragraph_Separator_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Paragraph_Separator_Set);
   function Control_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Control_Set);
   function Format_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Format_Set);
   function Private_Use_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Private_Use_Set);
   function Surrogate_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Surrogate_Set);

--  Control_Set : constant Wide_Wide_Character_Set;
   --  (Control_Set is declared as unicode category in above)
--  Graphic_Set : constant Wide_Wide_Character_Set;
   function Graphic_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Graphic_Set);
--  Letter_Set : constant Wide_Wide_Character_Set;
   function Letter_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Letter_Set);
--  Lower_Set : constant Wide_Wide_Character_Set;
   function Lower_Set return Wide_Wide_Character_Set
      renames Lowercase_Letter_Set;
   --  (Lower_Set is extended for all unicode characters)
--  Upper_Set : constant Wide_Wide_Character_Set;
   function Upper_Set return Wide_Wide_Character_Set
      renames Uppercase_Letter_Set;
   --  (Upper_Set is extended for all unicode characters)
--  Basic_Set : constant Wide_Wide_Character_Set;
   function Decimal_Digit_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Decimal_Digit_Set);
   function Hexadecimal_Digit_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Hexadecimal_Digit_Set);
   --  (Decimal_Digit_Set, Hexadecimal_Digit_Set are NOT extended, for parsing)
--  Alphanumeric_Set : constant Wide_Wide_Character_Set;
   function Alphanumeric_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Alphanumeric_Set);
--  Special_Set : constant Wide_Wide_Character_Set;
   function Special_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Special_Set);
--  ISO_646_Set : constant Wide_Wide_Character_Set;
   function ISO_646_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (ISO_646_Set);

--  Lower_Case_Map : constant Wide_Wide_Character_Mapping;
   function Lower_Case_Map return Wide_Wide_Character_Mapping;
   --  pragma Inline_Always (Lower_Case_Map);
   --  Maps to lower case for letters, else identity
   --  (Lower_Case_Map is extended for all unicode characters)
--  Upper_Case_Map : constant Wide_Wide_Character_Mapping;
   function Upper_Case_Map return Wide_Wide_Character_Mapping;
   --  pragma Inline_Always (Upper_Case_Map);
   --  Maps to upper case for letters, else identity
   --  (Upper_Case_Map is extended for all unicode characters)
--  Basic_Map : constant Wide_Wide_Character_Mapping;
   --  Maps to basic letter for letters, else identity

   --  extended
   function Case_Folding_Map return Wide_Wide_Character_Mapping;
   --  pragma Inline_Always (Case_Folding_Map);

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
   function Wide_Character_Set return Wide_Wide_Character_Set;
   --  pragma Inline_Always (Wide_Character_Set);
   --  (Wide_Character_Set is excluded surrogate pair)

private

   function Unassigned_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.All_Unassigned);
   function Unassigned_Set return Wide_Wide_Character_Set
      renames Unassigned_Set_Body;

   function Uppercase_Letter_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Uppercase_Letter);
   function Uppercase_Letter_Set return Wide_Wide_Character_Set
      renames Uppercase_Letter_Set_Body;

   function Lowercase_Letter_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Lowercase_Letter);
   function Lowercase_Letter_Set return Wide_Wide_Character_Set
      renames Lowercase_Letter_Set_Body;

   function Titlecase_Letter_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Titlecase_Letter);
   function Titlecase_Letter_Set return Wide_Wide_Character_Set
      renames Titlecase_Letter_Set_Body;

   function Modifier_Letter_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Modifier_Letter);
   function Modifier_Letter_Set return Wide_Wide_Character_Set
      renames Modifier_Letter_Set_Body;

   function Other_Letter_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Other_Letter);
   function Other_Letter_Set return Wide_Wide_Character_Set
      renames Other_Letter_Set_Body;

   function Decimal_Number_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Decimal_Number);
   function Decimal_Number_Set return Wide_Wide_Character_Set
      renames Decimal_Number_Set_Body;

   function Letter_Number_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Letter_Number);
   function Letter_Number_Set return Wide_Wide_Character_Set
      renames Letter_Number_Set_Body;

   function Other_Number_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Other_Number);
   function Other_Number_Set return Wide_Wide_Character_Set
      renames Other_Number_Set_Body;

   function Line_Separator_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Line_Separator);
   function Line_Separator_Set return Wide_Wide_Character_Set
      renames Line_Separator_Set_Body;

   function Paragraph_Separator_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Paragraph_Separator);
   function Paragraph_Separator_Set return Wide_Wide_Character_Set
      renames Paragraph_Separator_Set_Body;

   function Control_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Control);
   function Control_Set return Wide_Wide_Character_Set
      renames Control_Set_Body;

   function Format_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Format);
   function Format_Set return Wide_Wide_Character_Set
      renames Format_Set_Body;

   function Private_Use_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Private_Use);
   function Private_Use_Set return Wide_Wide_Character_Set
      renames Private_Use_Set_Body;

   function Surrogate_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.General_Category.Surrogate);
   function Surrogate_Set return Wide_Wide_Character_Set
      renames Surrogate_Set_Body;

   function Graphic_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Graphic_Set);
   function Graphic_Set return Wide_Wide_Character_Set
      renames Graphic_Set_Body;

   function Letter_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Letter_Set);
   function Letter_Set return Wide_Wide_Character_Set
      renames Letter_Set_Body;

   function Decimal_Digit_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Decimal_Digit_Set);
   function Decimal_Digit_Set return Wide_Wide_Character_Set
      renames Decimal_Digit_Set_Body;

   function Hexadecimal_Digit_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Hexadecimal_Digit_Set);
   function Hexadecimal_Digit_Set return Wide_Wide_Character_Set
      renames Hexadecimal_Digit_Set_Body;

   function Alphanumeric_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Alphanumeric_Set);
   function Alphanumeric_Set return Wide_Wide_Character_Set
      renames Alphanumeric_Set_Body;

   function Special_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Special_Set);
   function Special_Set return Wide_Wide_Character_Set
      renames Special_Set_Body;

   function ISO_646_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.ISO_646_Set);
   function ISO_646_Set return Wide_Wide_Character_Set
      renames ISO_646_Set_Body;

   function Lower_Case_Map_Body is new Root_Maps.Inside.To_Mapping (
      Wide_Wide_Character_Mapping,
      Characters.Inside.Maps.Lower_Case.Lower_Case_Map);
   function Lower_Case_Map return Wide_Wide_Character_Mapping
      renames Lower_Case_Map_Body;

   function Upper_Case_Map_Body is new Root_Maps.Inside.To_Mapping (
      Wide_Wide_Character_Mapping,
      Characters.Inside.Maps.Upper_Case.Upper_Case_Map);
   function Upper_Case_Map return Wide_Wide_Character_Mapping
      renames Upper_Case_Map_Body;

   function Case_Folding_Map_Body is new Root_Maps.Inside.To_Mapping (
      Wide_Wide_Character_Mapping,
      Characters.Inside.Maps.Case_Folding.Case_Folding_Map);
   function Case_Folding_Map return Wide_Wide_Character_Mapping
      renames Case_Folding_Map_Body;

   function Wide_Character_Set_Body is new Root_Maps.Inside.To_Set (
      Wide_Wide_Character_Set,
      Characters.Inside.Sets.Constants.Wide_Character_Set);
   function Wide_Character_Set return Wide_Wide_Character_Set
      renames Wide_Character_Set_Body;

end Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
