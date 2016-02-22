pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.General_Category is
   pragma Preelaborate;

   --  General_Category=Unassigned (Cn)
   function Unassigned return not null Character_Set_Access;
   function All_Unassigned return not null Character_Set_Access;
      --  Contains Unassigned + (16#110000# .. Wide_Wide_Character'Last).

   --  General_Category=Uppercase_Letter (Lu)
   function Uppercase_Letter return not null Character_Set_Access;

   --  General_Category=Lowercase_Letter (Ll)
   function Lowercase_Letter return not null Character_Set_Access;

   --  General_Category=Titlecase_Letter (Lt)
   function Titlecase_Letter return not null Character_Set_Access;

   --  General_Category=Modifier_Letter (Lm)
   function Modifier_Letter return not null Character_Set_Access;

   --  General_Category=Other_Letter (Lo)
   function Other_Letter return not null Character_Set_Access;

   --  General_Category=Nonspacing_Mark (Mn)
   function Nonspacing_Mark return not null Character_Set_Access;

   --  General_Category=Enclosing_Mark (Me)
   function Enclosing_Mark return not null Character_Set_Access;

   --  General_Category=Spacing_Mark (Mc)
   function Spacing_Mark return not null Character_Set_Access;

   --  General_Category=Decimal_Number (Nd)
   function Decimal_Number return not null Character_Set_Access;

   --  General_Category=Letter_Number (Nl)
   function Letter_Number return not null Character_Set_Access;

   --  General_Category=Other_Number (No)
   function Other_Number return not null Character_Set_Access;

   --  General_Category=Space_Separator (Zs)
   function Space_Separator return not null Character_Set_Access;

   --  General_Category=Line_Separator (Zl)
   function Line_Separator return not null Character_Set_Access;

   --  General_Category=Paragraph_Separator (Zp)
   function Paragraph_Separator return not null Character_Set_Access;

   --  General_Category=Control (Cc)
   function Control return not null Character_Set_Access;

   --  General_Category=Format (Cf)
   function Format return not null Character_Set_Access;

   --  General_Category=Private_Use (Co)
   function Private_Use return not null Character_Set_Access;

   --  General_Category=Surrogate (Cs)
   function Surrogate return not null Character_Set_Access;

   --  General_Category=Dash_Punctuation (Pd)
   function Dash_Punctuation return not null Character_Set_Access;

   --  General_Category=Open_Punctuation (Ps)
   function Open_Punctuation return not null Character_Set_Access;

   --  General_Category=Close_Punctuation (Pe)
   function Close_Punctuation return not null Character_Set_Access;

   --  General_Category=Connector_Punctuation (Pc)
   function Connector_Punctuation return not null Character_Set_Access;

   --  General_Category=Other_Punctuation (Po)
   function Other_Punctuation return not null Character_Set_Access;

   --  General_Category=Math_Symbol (Sm)
   function Math_Symbol return not null Character_Set_Access;

   --  General_Category=Currency_Symbol (Sc)
   function Currency_Symbol return not null Character_Set_Access;

   --  General_Category=Modifier_Symbol (Sk)
   function Modifier_Symbol return not null Character_Set_Access;

   --  General_Category=Other_Symbol (So)
   function Other_Symbol return not null Character_Set_Access;

   --  General_Category=Initial_Punctuation (Pi)
   function Initial_Punctuation return not null Character_Set_Access;

   --  General_Category=Final_Punctuation (Pf)
   function Final_Punctuation return not null Character_Set_Access;

end Ada.Strings.Naked_Maps.General_Category;
