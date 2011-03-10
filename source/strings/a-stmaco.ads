pragma License (Unrestricted);
with Ada.Characters.Inside.Maps;
with Ada.Characters.Inside.Maps.Case_Folding;
with Ada.Characters.Inside.Maps.Lower_Case;
with Ada.Characters.Inside.Maps.Upper_Case;
with Ada.Characters.Inside.Sets;
with Ada.Characters.Inside.Sets.Constants;
with Ada.Characters.Inside.Sets.General_Category;
with Ada.Strings.Root_Maps.Inside;
package Ada.Strings.Maps.Constants is
--  pragma Pure;
   pragma Preelaborate; -- controlled types

   --  extended sets of unicode category
   function Unassigned_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.All_Unassigned);
   function Uppercase_Letter_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Uppercase_Letter);
   function Lowercase_Letter_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Lowercase_Letter);
   function Titlecase_Letter_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Titlecase_Letter);
   function Modifier_Letter_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Modifier_Letter);
   function Other_Letter_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Other_Letter);
   function Decimal_Number_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Decimal_Number);
   function Letter_Number_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Letter_Number);
   function Other_Number_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Other_Number);
   function Line_Separator_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Line_Separator);
   function Paragraph_Separator_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Paragraph_Separator);
   function Control_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Control);
   function Format_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Format);
   function Private_Use_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Private_Use);
   function Surrogate_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.General_Category.Surrogate);

   --  relation of constants for Latin-1 in RM A.3.2
   --
   --  + all characters
   --     + Graphic
   --        + Alphanumeric
   --           + Letter
   --              + Lower
   --              + Upper
   --           + Decimal_Digit ('0' .. '9')
   --        + Special (Graphic - Alphanumeric)
   --     + Control
   --  * Hexadecimal_Digit = Decimal_Digit + ('A' .. 'F' | 'a' .. 'f')
   --  * Basic = Letter without modifier(s)
   --
   --  extended for Unicode
   --
   --  + all characters
   --     + Unassigned (Cn + (16#110000# .. Wide_Wide_Character'Last))
   --     + Graphic
   --        + Alphanumeric
   --           + Letter
   --              + Lower (Ll)
   --              + Upper (Lu)
   --              + Titlecase_Letter (Lt)
   --              + Modifier_Letter (Lm)
   --              + Other_Letter (Lo)
   --           + Decimal_Number (Nd)
   --              + Decimal_Digit ('0' .. '9')
   --           + Letter_Number (Nl)
   --           + Other_Number (No)
   --        + Special
   --           (Mn, Me, Mc, Zs, Pd, Ps, Pe, Pc, Po, Sm, Sc, Sk, So, Pi, Pf)
   --     + Line_Separator (Zl)
   --     + Paragraph_Separator (Zp)
   --     + Control (Cc)
   --     + Format (Cf)
   --     + Private_Use (Co)
   --     + Surrogate (Cs)
   --  * Hexadecimal_Digit = Decimal_Digit + ('A' .. 'F' | 'a' .. 'f')
   --  * Basic = unimplemented

--  Control_Set : constant Character_Set;
   --  (Control_Set is declared as unicode category in above)
--  Graphic_Set : constant Character_Set;
   function Graphic_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.Constants.Graphic_Set);
--  Letter_Set : constant Character_Set;
   function Letter_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.Constants.Letter_Set);
--  Lower_Set : constant Character_Set;
   function Lower_Set return Character_Set
      renames Lowercase_Letter_Set;
   --  (Lower_Set is extended for all unicode characters)
--  Upper_Set : constant Character_Set;
   function Upper_Set return Character_Set
      renames Uppercase_Letter_Set;
   --  (Upper_Set is extended for all unicode characters)
--  Basic_Set : constant Character_Set;
   function Decimal_Digit_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.Constants.Decimal_Digit_Set);
   function Hexadecimal_Digit_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.Constants.Hexadecimal_Digit_Set);
   --  (Decimal_Digit_Set, Hexadecimal_Digit_Set are NOT extended, for parsing)
--  Alphanumeric_Set : constant Character_Set;
   function Alphanumeric_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.Constants.Alphanumeric_Set);
--  Special_Set : constant Character_Set;
   function Special_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.Constants.Special_Set);
--  ISO_646_Set : constant Character_Set;
   function ISO_646_Set is new Strings.Root_Maps.Inside.To_Set (
      Character_Set,
      Characters.Inside.Sets.Constants.ISO_646_Set);

--  Lower_Case_Map : constant Character_Mapping;
   function Lower_Case_Map is new Strings.Root_Maps.Inside.To_Mapping (
      Character_Mapping,
      Characters.Inside.Maps.Lower_Case.Lower_Case_Map);
   --  Maps to lower case for letters, else identity
   --  (Lower_Case_Map is extended for all unicode characters)
--  Upper_Case_Map : constant Character_Mapping;
   function Upper_Case_Map is new Strings.Root_Maps.Inside.To_Mapping (
      Character_Mapping,
      Characters.Inside.Maps.Upper_Case.Upper_Case_Map);
   --  Maps to upper case for letters, else identity
   --  (Upper_Case_Map is extended for all unicode characters)
--  Basic_Map : constant Character_Mapping;
   --  Maps to basic letter for letters, else identity

   --  extended
   function Case_Folding_Map is new Strings.Root_Maps.Inside.To_Mapping (
      Character_Mapping,
      Characters.Inside.Maps.Case_Folding.Case_Folding_Map);

end Ada.Strings.Maps.Constants;
