pragma License (Unrestricted);
private with Ada.Strings.Maps.Naked;
private with Ada.Strings.Naked_Maps.Case_Folding;
private with Ada.Strings.Naked_Maps.Case_Mapping;
private with Ada.Strings.Naked_Maps.General_Category;
private with Ada.Strings.Naked_Maps.Set_Constants;
package Ada.Strings.Maps.Constants is
--  pragma Pure;
   pragma Preelaborate; -- controlled types

   --  This implementation is heavy for size of the executable file,
   --    because all UCD table may be linked. It should be separating...
   --  Inlining that's deeper than 3 (-O2 -gnatn) may exclude
   --    the object code of this file from linking.

   --  extended
   --  There are sets of unicode category.
   function Unassigned_Set return Character_Set;
   function Uppercase_Letter_Set return Character_Set;
   function Lowercase_Letter_Set return Character_Set;
   function Titlecase_Letter_Set return Character_Set;
   function Modifier_Letter_Set return Character_Set;
   function Other_Letter_Set return Character_Set;
   function Decimal_Number_Set return Character_Set;
   function Letter_Number_Set return Character_Set;
   function Other_Number_Set return Character_Set;
   function Line_Separator_Set return Character_Set;
   function Paragraph_Separator_Set return Character_Set;
   function Control_Set return Character_Set;
   function Format_Set return Character_Set;
   function Private_Use_Set return Character_Set;
   function Surrogate_Set return Character_Set;

   pragma Inline (Unassigned_Set); -- renamed, the followings are the same
   pragma Inline (Uppercase_Letter_Set);
   pragma Inline (Lowercase_Letter_Set);
   pragma Inline (Titlecase_Letter_Set);
   pragma Inline (Modifier_Letter_Set);
   pragma Inline (Other_Letter_Set);
   pragma Inline (Decimal_Number_Set);
   pragma Inline (Letter_Number_Set);
   pragma Inline (Other_Number_Set);
   pragma Inline (Line_Separator_Set);
   pragma Inline (Paragraph_Separator_Set);
   pragma Inline (Control_Set);
   pragma Inline (Format_Set);
   pragma Inline (Private_Use_Set);
   pragma Inline (Surrogate_Set);

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
   --  constants are modified for Unicode
   --
   --  + all characters
   --     + Unassigned (Cn + (16#110000# .. Character'Last))
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
   function Graphic_Set return Character_Set;
--  Letter_Set : constant Character_Set;
   function Letter_Set return Character_Set;
--  Lower_Set : constant Character_Set;
   function Lower_Set return Character_Set
      renames Lowercase_Letter_Set;
   --  (Lower_Set is extended for all unicode characters)
--  Upper_Set : constant Character_Set;
   function Upper_Set return Character_Set
      renames Uppercase_Letter_Set;
   --  (Upper_Set is extended for all unicode characters)
--  Basic_Set : constant Character_Set;
   function Decimal_Digit_Set return Character_Set;
   function Hexadecimal_Digit_Set return Character_Set;
   --  (Decimal_Digit_Set, Hexadecimal_Digit_Set are NOT extended, for parsing)
--  Alphanumeric_Set : constant Character_Set;
   function Alphanumeric_Set return Character_Set;
--  Special_Set : constant Character_Set;
   function Special_Set return Character_Set;
--  ISO_646_Set : constant Character_Set;
   function ISO_646_Set return Character_Set;

   pragma Inline (Graphic_Set);
   pragma Inline (Letter_Set);
   pragma Inline (Decimal_Digit_Set);
   pragma Inline (Hexadecimal_Digit_Set);
   pragma Inline (Alphanumeric_Set);
   pragma Inline (Special_Set);
   pragma Inline (ISO_646_Set);

--  Lower_Case_Map : constant Character_Mapping;
   function Lower_Case_Map return Character_Mapping;
   --  Maps to lower case for letters, else identity
   --  (Lower_Case_Map is extended for all unicode characters)
--  Upper_Case_Map : constant Character_Mapping;
   function Upper_Case_Map return Character_Mapping;
   --  Maps to upper case for letters, else identity
   --  (Upper_Case_Map is extended for all unicode characters)
--  Basic_Map : constant Character_Mapping;
   --  Maps to basic letter for letters, else identity
   --  extended
   function Case_Folding_Map return Character_Mapping;

   pragma Inline (Lower_Case_Map);
   pragma Inline (Upper_Case_Map);
   pragma Inline (Case_Folding_Map);

private

   function Unassigned_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.All_Unassigned);
   function Unassigned_Set return Character_Set
      renames Unassigned_Set_Body;

   function Uppercase_Letter_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Uppercase_Letter);
   function Uppercase_Letter_Set return Character_Set
      renames Uppercase_Letter_Set_Body;

   function Lowercase_Letter_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Lowercase_Letter);
   function Lowercase_Letter_Set return Character_Set
      renames Lowercase_Letter_Set_Body;

   function Titlecase_Letter_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Titlecase_Letter);
   function Titlecase_Letter_Set return Character_Set
      renames Titlecase_Letter_Set_Body;

   function Modifier_Letter_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Modifier_Letter);
   function Modifier_Letter_Set return Character_Set
      renames Modifier_Letter_Set_Body;

   function Other_Letter_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Other_Letter);
   function Other_Letter_Set return Character_Set
      renames Other_Letter_Set_Body;

   function Decimal_Number_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Decimal_Number);
   function Decimal_Number_Set return Character_Set
      renames Decimal_Number_Set_Body;

   function Letter_Number_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Letter_Number);
   function Letter_Number_Set return Character_Set
      renames Letter_Number_Set_Body;

   function Other_Number_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Other_Number);
   function Other_Number_Set return Character_Set
      renames Other_Number_Set_Body;

   function Line_Separator_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Line_Separator);
   function Line_Separator_Set return Character_Set
      renames Line_Separator_Set_Body;

   function Paragraph_Separator_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Paragraph_Separator);
   function Paragraph_Separator_Set return Character_Set
      renames Paragraph_Separator_Set_Body;

   function Control_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Control);
   function Control_Set return Character_Set
      renames Control_Set_Body;

   function Format_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Format);
   function Format_Set return Character_Set
      renames Format_Set_Body;

   function Private_Use_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Private_Use);
   function Private_Use_Set return Character_Set
      renames Private_Use_Set_Body;

   function Surrogate_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.General_Category.Surrogate);
   function Surrogate_Set return Character_Set
      renames Surrogate_Set_Body;

   function Graphic_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.Set_Constants.Graphic_Set);
   function Graphic_Set return Character_Set
      renames Graphic_Set_Body;

   function Letter_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.Set_Constants.Letter_Set);
   function Letter_Set return Character_Set
      renames Letter_Set_Body;

   function Decimal_Digit_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.Set_Constants.Decimal_Digit_Set);
   function Decimal_Digit_Set return Character_Set
      renames Decimal_Digit_Set_Body;

   function Hexadecimal_Digit_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.Set_Constants.Hexadecimal_Digit_Set);
   function Hexadecimal_Digit_Set return Character_Set
      renames Hexadecimal_Digit_Set_Body;

   function Alphanumeric_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.Set_Constants.Alphanumeric_Set);
   function Alphanumeric_Set return Character_Set
      renames Alphanumeric_Set_Body;

   function Special_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.Set_Constants.Special_Set);
   function Special_Set return Character_Set
      renames Special_Set_Body;

   function ISO_646_Set_Body is
      new Maps.Naked.To_Set (Naked_Maps.Set_Constants.ISO_646_Set);
   function ISO_646_Set return Character_Set
      renames ISO_646_Set_Body;

   function Lower_Case_Map_Body is
      new Maps.Naked.To_Mapping (Naked_Maps.Case_Mapping.Lower_Case_Map);
   function Lower_Case_Map return Character_Mapping
      renames Lower_Case_Map_Body;

   function Upper_Case_Map_Body is
      new Maps.Naked.To_Mapping (Naked_Maps.Case_Mapping.Upper_Case_Map);
   function Upper_Case_Map return Character_Mapping
      renames Upper_Case_Map_Body;

   function Case_Folding_Map_Body is
      new Maps.Naked.To_Mapping (Naked_Maps.Case_Folding.Case_Folding_Map);
   function Case_Folding_Map return Character_Mapping
      renames Case_Folding_Map_Body;

end Ada.Strings.Maps.Constants;
