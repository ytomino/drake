with Ada.Strings.Naked_Maps.General_Category;
with System.Once;
with System.Reference_Counting;
package body Ada.Strings.Naked_Maps.Set_Constants is

   type Character_Set_Access is access Naked_Maps.Character_Set;

   function Total_Length (Source : Character_Ranges_Array) return Natural;
   function Total_Length (Source : Character_Ranges_Array) return Natural is
      Result : Natural := 0;
   begin
      for I in Source'Range loop
         Result := Result + Source (I)'Length;
      end loop;
      return Result;
   end Total_Length;

   --  implementation

   Decimal_Digit_Set_Data : Character_Set_Access;
   Decimal_Digit_Flag : aliased System.Once.Flag := 0;

   procedure Decimal_Digit_Init;
   procedure Decimal_Digit_Init is
   begin
      Decimal_Digit_Set_Data := new Naked_Maps.Character_Set'(
         Length => 1,
         Reference_Count => System.Reference_Counting.Static,
         Items => (1 => ('0', '9')));
   end Decimal_Digit_Init;

   function Decimal_Digit_Set
      return not null access Naked_Maps.Character_Set is
   begin
      System.Once.Initialize (
         Decimal_Digit_Flag'Access,
         Decimal_Digit_Init'Access);
      return Decimal_Digit_Set_Data;
   end Decimal_Digit_Set;

   Hexadecimal_Digit_Set_Data : Character_Set_Access;
   Hexadecimal_Digit_Flag : aliased System.Once.Flag := 0;

   procedure Hexadecimal_Digit_Init;
   procedure Hexadecimal_Digit_Init is
   begin
      Hexadecimal_Digit_Set_Data :=
         new Naked_Maps.Character_Set'(
            Length => 3,
            Reference_Count => System.Reference_Counting.Static,
            Items => (('0', '9'), ('A', 'F'), ('a', 'f')));
   end Hexadecimal_Digit_Init;

   function Hexadecimal_Digit_Set
      return not null access Naked_Maps.Character_Set is
   begin
      System.Once.Initialize (
         Hexadecimal_Digit_Flag'Access,
         Hexadecimal_Digit_Init'Access);
      return Hexadecimal_Digit_Set_Data;
   end Hexadecimal_Digit_Set;

   ISO_646_Set_Data : Character_Set_Access;
   ISO_646_Flag : aliased System.Once.Flag := 0;

   procedure ISO_646_Init;
   procedure ISO_646_Init is
   begin
      ISO_646_Set_Data := new Naked_Maps.Character_Set'(
         Length => 1,
         Reference_Count => System.Reference_Counting.Static,
         Items => (1 => (
            Character_Type'Val (0),
            Character_Type'Val (16#7F#))));
   end ISO_646_Init;

   function ISO_646_Set return not null access Naked_Maps.Character_Set is
   begin
      System.Once.Initialize (
         ISO_646_Flag'Access,
         ISO_646_Init'Access);
      return ISO_646_Set_Data;
   end ISO_646_Set;

   Wide_Character_Set_Data : Character_Set_Access;
   Wide_Character_Flag : aliased System.Once.Flag := 0;

   procedure Wide_Character_Init;
   procedure Wide_Character_Init is
   begin
      Wide_Character_Set_Data := new Naked_Maps.Character_Set'(
         Length => 2,
         Reference_Count => System.Reference_Counting.Static,
         Items => (
            1 => (
               Character_Type'Val (0),
               Character_Type'Val (16#D7FF#)),
            2 => (
               Character_Type'Val (16#E000#),
               Character_Type'Val (16#FFFF#))));
   end Wide_Character_Init;

   function Wide_Character_Set
      return not null access Naked_Maps.Character_Set is
   begin
      System.Once.Initialize (
         Wide_Character_Flag'Access,
         Wide_Character_Init'Access);
      return Wide_Character_Set_Data;
   end Wide_Character_Set;

   Letter_Set_Data : Character_Set_Access;
   Letter_Flag : aliased System.Once.Flag := 0;

   procedure Letter_Init;
   procedure Letter_Init is
      Source : constant Character_Ranges_Array := (
         General_Category.Lowercase_Letter.Items'Unrestricted_Access,
         General_Category.Uppercase_Letter.Items'Unrestricted_Access,
         General_Category.Titlecase_Letter.Items'Unrestricted_Access,
         General_Category.Modifier_Letter.Items'Unrestricted_Access,
         General_Category.Other_Letter.Items'Unrestricted_Access);
      Items : Character_Ranges (1 .. Total_Length (Source));
      Last : Natural;
   begin
      Merge (Items, Last, Source);
      Letter_Set_Data := new Naked_Maps.Character_Set'(
         Length => Last,
         Reference_Count => System.Reference_Counting.Static,
         Items => Items (1 .. Last));
   end Letter_Init;

   function Letter_Set return not null access Naked_Maps.Character_Set is
   begin
      System.Once.Initialize (
         Letter_Flag'Access,
         Letter_Init'Access);
      return Letter_Set_Data;
   end Letter_Set;

   Alphanumeric_Set_Data : Character_Set_Access;
   Alphanumeric_Flag : aliased System.Once.Flag := 0;

   procedure Alphanumeric_Init;
   procedure Alphanumeric_Init is
      Source : constant Character_Ranges_Array := (
         Letter_Set.Items'Unrestricted_Access,
         General_Category.Decimal_Number.Items'Unrestricted_Access,
         General_Category.Letter_Number.Items'Unrestricted_Access,
         General_Category.Other_Number.Items'Unrestricted_Access);
      Items : Character_Ranges (1 .. Total_Length (Source));
      Last : Natural;
   begin
      Merge (Items, Last, Source);
      Alphanumeric_Set_Data := new Naked_Maps.Character_Set'(
         Length => Last,
         Reference_Count => System.Reference_Counting.Static,
         Items => Items (1 .. Last));
   end Alphanumeric_Init;

   function Alphanumeric_Set return not null access Naked_Maps.Character_Set is
   begin
      System.Once.Initialize (
         Alphanumeric_Flag'Access,
         Alphanumeric_Init'Access);
      return Alphanumeric_Set_Data;
   end Alphanumeric_Set;

   Special_Set_Data : Character_Set_Access := null;
   Special_Flag : aliased System.Once.Flag := 0;

   procedure Special_Init;
   procedure Special_Init is
      Source : constant Character_Ranges_Array := (
         General_Category.Nonspacing_Mark.Items'Unrestricted_Access,
         General_Category.Enclosing_Mark.Items'Unrestricted_Access,
         General_Category.Spacing_Mark.Items'Unrestricted_Access,
         General_Category.Space_Separator.Items'Unrestricted_Access,
         General_Category.Dash_Punctuation.Items'Unrestricted_Access,
         General_Category.Open_Punctuation.Items'Unrestricted_Access,
         General_Category.Close_Punctuation.Items'Unrestricted_Access,
         General_Category.Connector_Punctuation.
            Items'Unrestricted_Access,
         General_Category.Other_Punctuation.Items'Unrestricted_Access,
         General_Category.Math_Symbol.Items'Unrestricted_Access,
         General_Category.Currency_Symbol.Items'Unrestricted_Access,
         General_Category.Modifier_Symbol.Items'Unrestricted_Access,
         General_Category.Other_Symbol.Items'Unrestricted_Access,
         General_Category.Initial_Punctuation.Items'Unrestricted_Access,
         General_Category.Final_Punctuation.Items'Unrestricted_Access);
      Items : Character_Ranges (1 .. Total_Length (Source));
      Last : Natural;
   begin
      Merge (Items, Last, Source);
      Special_Set_Data := new Naked_Maps.Character_Set'(
         Length => Last,
         Reference_Count => System.Reference_Counting.Static,
         Items => Items (1 .. Last));
   end Special_Init;

   function Special_Set return not null access Naked_Maps.Character_Set is
   begin
      System.Once.Initialize (
         Special_Flag'Access,
         Special_Init'Access);
      return Special_Set_Data;
   end Special_Set;

   Graphic_Set_Data : Character_Set_Access := null;
   Graphic_Flag : aliased System.Once.Flag := 0;

   procedure Graphic_Init;
   procedure Graphic_Init is
      Source : constant Character_Ranges_Array := (
         Alphanumeric_Set.Items'Unrestricted_Access,
         Special_Set.Items'Unrestricted_Access);
      Items : Character_Ranges (1 .. Total_Length (Source));
      Last : Natural;
   begin
      Merge (Items, Last, Source);
      Graphic_Set_Data := new Naked_Maps.Character_Set'(
         Length => Last,
         Reference_Count => System.Reference_Counting.Static,
         Items => Items (1 .. Last));
   end Graphic_Init;

   function Graphic_Set return not null access Naked_Maps.Character_Set is
   begin
      System.Once.Initialize (
         Graphic_Flag'Access,
         Graphic_Init'Access);
      return Graphic_Set_Data;
   end Graphic_Set;

end Ada.Strings.Naked_Maps.Set_Constants;
