with Ada.UCD.General_Category;
with System.Once;
with System.Reference_Counting;
package body Ada.Characters.Inside.Sets.General_Category is
   use type UCD.Difference_Base;

   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.UCS_2_Array;
      Offset : UCD.Difference_Base);
   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.UCS_2_Array;
      Offset : UCD.Difference_Base) is
   begin
      pragma Assert (Table'Length = To'Length);
      for I in Table'Range loop
         declare
            T : Character_Range
               renames To (To'First - Table'First + I);
            C : constant Wide_Wide_Character := Wide_Wide_Character'Val (
               UCD.Difference_Base (Table (I)) + Offset);
         begin
            T.Low := C;
            T.High := C;
         end;
      end loop;
   end Fill;

   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.UCS_4_Array);
   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.UCS_4_Array) is
   begin
      pragma Assert (Table'Length = To'Length);
      for I in Table'Range loop
         declare
            T : Character_Range
               renames To (To'First - Table'First + I);
            C : constant Wide_Wide_Character := Wide_Wide_Character'Val (
               Table (I));
         begin
            T.Low := C;
            T.High := C;
         end;
      end loop;
   end Fill;

   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.Set_16_Type;
      Offset : UCD.Difference_Base);
   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.Set_16_Type;
      Offset : UCD.Difference_Base) is
   begin
      pragma Assert (Table'Length = To'Length);
      for I in Table'Range loop
         declare
            T : Character_Range
               renames To (To'First - Table'First + I);
         begin
            T.Low := Wide_Wide_Character'Val (
               UCD.Difference_Base (Table (I).Low) + Offset);
            T.High := Wide_Wide_Character'Val (
               UCD.Difference_Base (Table (I).High) + Offset);
         end;
      end loop;
   end Fill;

   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.Set_32_Type);
   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.Set_32_Type) is
   begin
      pragma Assert (Table'Length = To'Length);
      for I in Table'Range loop
         declare
            T : Character_Range
               renames To (To'First - Table'First + I);
         begin
            T.Low := Wide_Wide_Character'Val (Table (I).Low);
            T.High := Wide_Wide_Character'Val (Table (I).High);
         end;
      end loop;
   end Fill;

   procedure Fill (
      To : in out Character_Ranges;
      Table_16x1 : UCD.UCS_2_Array;
      Table_16x2 : UCD.Set_16_Type;
      Table_17x1 : UCD.UCS_2_Array;
      Table_17x2 : UCD.Set_16_Type;
      Table_32x1 : UCD.UCS_4_Array;
      Table_32x2 : UCD.Set_32_Type);
   procedure Fill (
      To : in out Character_Ranges;
      Table_16x1 : UCD.UCS_2_Array;
      Table_16x2 : UCD.Set_16_Type;
      Table_17x1 : UCD.UCS_2_Array;
      Table_17x2 : UCD.Set_16_Type;
      Table_32x1 : UCD.UCS_4_Array;
      Table_32x2 : UCD.Set_32_Type)
   is
      T_16x1 : Character_Ranges (Table_16x1'Range);
      T_16x2 : Character_Ranges (Table_16x2'Range);
      T_17x1 : Character_Ranges (Table_17x1'Range);
      T_17x2 : Character_Ranges (Table_17x2'Range);
      T_32x1 : Character_Ranges (Table_32x1'Range);
      T_32x2 : Character_Ranges (Table_32x2'Range);
      R_16_First : constant Positive := To'First;
      R_17_First : constant Positive :=
         R_16_First + Table_16x1'Length + Table_16x2'Length;
      R_32_First : constant Positive :=
         R_17_First + Table_17x1'Length + Table_17x2'Length;
      R_32_Last : constant Natural :=
         R_32_First + Table_32x1'Length + Table_32x2'Length - 1;
      Last : Natural;
   begin
      Fill (T_16x1, Table_16x1, Offset => 0);
      Fill (T_16x2, Table_16x2, Offset => 0);
      Merge (To (R_16_First .. R_17_First - 1), Last, T_16x1, T_16x2);
      pragma Assert (Last = R_17_First - 1);
      Fill (T_17x1, Table_17x1, Offset => 16#10000#);
      Fill (T_17x2, Table_17x2, Offset => 16#10000#);
      Merge (To (R_17_First .. R_32_First - 1), Last, T_17x1, T_17x2);
      pragma Assert (Last = R_32_First - 1);
      Fill (T_32x1, Table_32x1);
      Fill (T_32x2, Table_32x2);
      Merge (To (R_32_First .. R_32_Last), Last, T_32x1, T_32x2);
      pragma Assert (Last = R_32_Last);
      pragma Assert (Last = To'Last);
   end Fill;

   type Character_Set_Access is access Character_Set;

   --  General_Category=Unassigned (Cn)

   Unassigned_Set : Character_Set_Access := null;
   Unassigned_Flag : aliased System.Once.Flag := 0;

   procedure Unassigned_Init;
   procedure Unassigned_Init is
   begin
      Unassigned_Set := new Character_Set'(
         Length => UCD.General_Category.Cn_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Unassigned_Set.Items,
         UCD.General_Category.Cn_Table_XXXXx1,
         UCD.General_Category.Cn_Table_XXXXx2,
         UCD.General_Category.Cn_Table_1XXXXx1,
         UCD.General_Category.Cn_Table_1XXXXx2,
         UCD.General_Category.Cn_Table_XXXXXXXXx1,
         UCD.General_Category.Cn_Table_XXXXXXXXx2);
   end Unassigned_Init;

   function Unassigned return not null access Character_Set is
   begin
      System.Once.Initialize (
         Unassigned_Flag'Access,
         Unassigned_Init'Access);
      return Unassigned_Set;
   end Unassigned;

   All_Unassigned_Set : Character_Set_Access := null;
   All_Unassigned_Flag : aliased System.Once.Flag := 0;

   procedure All_Unassigned_Init;
   procedure All_Unassigned_Init is
   begin
      declare
         UA : constant not null access Character_Set := Unassigned;
      begin
         All_Unassigned_Set := new Character_Set'(
            Length => UA.Length,
            Reference_Count => System.Reference_Counting.Static,
            Items => UA.Items);
      end;
      pragma Assert (
         All_Unassigned_Set.Items (All_Unassigned_Set.Items'Last).High =
         Character_Type'Val (16#10FFFF#));
      All_Unassigned_Set.Items (All_Unassigned_Set.Items'Last).High :=
         Character_Type'Last;
   end All_Unassigned_Init;

   function All_Unassigned return not null access Character_Set is
   begin
      System.Once.Initialize (
         All_Unassigned_Flag'Access,
         All_Unassigned_Init'Access);
      return All_Unassigned_Set;
   end All_Unassigned;

   --  General_Category=Uppercase_Letter (Lu)

   Uppercase_Letter_Set : Character_Set_Access := null;
   Uppercase_Letter_Flag : aliased System.Once.Flag := 0;

   procedure Uppercase_Letter_Init;
   procedure Uppercase_Letter_Init is
   begin
      Uppercase_Letter_Set := new Character_Set'(
         Length => UCD.General_Category.Lu_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Uppercase_Letter_Set.Items,
         UCD.General_Category.Lu_Table_XXXXx1,
         UCD.General_Category.Lu_Table_XXXXx2,
         UCD.General_Category.Lu_Table_1XXXXx1,
         UCD.General_Category.Lu_Table_1XXXXx2,
         UCD.General_Category.Lu_Table_XXXXXXXXx1,
         UCD.General_Category.Lu_Table_XXXXXXXXx2);
   end Uppercase_Letter_Init;

   function Uppercase_Letter return not null access Character_Set is
   begin
      System.Once.Initialize (
         Uppercase_Letter_Flag'Access,
         Uppercase_Letter_Init'Access);
      return Uppercase_Letter_Set;
   end Uppercase_Letter;

   --  General_Category=Lowercase_Letter (Ll)

   Lowercase_Letter_Set : Character_Set_Access := null;
   Lowercase_Letter_Flag : aliased System.Once.Flag := 0;

   procedure Lowercase_Letter_Init;
   procedure Lowercase_Letter_Init is
   begin
      Lowercase_Letter_Set := new Character_Set'(
         Length => UCD.General_Category.Ll_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Lowercase_Letter_Set.Items,
         UCD.General_Category.Ll_Table_XXXXx1,
         UCD.General_Category.Ll_Table_XXXXx2,
         UCD.General_Category.Ll_Table_1XXXXx1,
         UCD.General_Category.Ll_Table_1XXXXx2,
         UCD.General_Category.Ll_Table_XXXXXXXXx1,
         UCD.General_Category.Ll_Table_XXXXXXXXx2);
   end Lowercase_Letter_Init;

   function Lowercase_Letter return not null access Character_Set is
   begin
      System.Once.Initialize (
         Lowercase_Letter_Flag'Access,
         Lowercase_Letter_Init'Access);
      return Lowercase_Letter_Set;
   end Lowercase_Letter;

   --  General_Category=Titlecase_Letter (Lt)

   Titlecase_Letter_Set : Character_Set_Access := null;
   Titlecase_Letter_Flag : aliased System.Once.Flag := 0;

   procedure Titlecase_Letter_Init;
   procedure Titlecase_Letter_Init is
   begin
      Titlecase_Letter_Set := new Character_Set'(
         Length => UCD.General_Category.Lt_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Titlecase_Letter_Set.Items,
         UCD.General_Category.Lt_Table_XXXXx1,
         UCD.General_Category.Lt_Table_XXXXx2,
         UCD.General_Category.Lt_Table_1XXXXx1,
         UCD.General_Category.Lt_Table_1XXXXx2,
         UCD.General_Category.Lt_Table_XXXXXXXXx1,
         UCD.General_Category.Lt_Table_XXXXXXXXx2);
   end Titlecase_Letter_Init;

   function Titlecase_Letter return not null access Character_Set is
   begin
      System.Once.Initialize (
         Titlecase_Letter_Flag'Access,
         Titlecase_Letter_Init'Access);
      return Titlecase_Letter_Set;
   end Titlecase_Letter;

   --  General_Category=Modifier_Letter (Lm)

   Modifier_Letter_Set : Character_Set_Access := null;
   Modifier_Letter_Flag : aliased System.Once.Flag := 0;

   procedure Modifier_Letter_Init;
   procedure Modifier_Letter_Init is
   begin
      Modifier_Letter_Set := new Character_Set'(
         Length => UCD.General_Category.Lm_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Modifier_Letter_Set.Items,
         UCD.General_Category.Lm_Table_XXXXx1,
         UCD.General_Category.Lm_Table_XXXXx2,
         UCD.General_Category.Lm_Table_1XXXXx1,
         UCD.General_Category.Lm_Table_1XXXXx2,
         UCD.General_Category.Lm_Table_XXXXXXXXx1,
         UCD.General_Category.Lm_Table_XXXXXXXXx2);
   end Modifier_Letter_Init;

   function Modifier_Letter return not null access Character_Set is
   begin
      System.Once.Initialize (
         Modifier_Letter_Flag'Access,
         Modifier_Letter_Init'Access);
      return Modifier_Letter_Set;
   end Modifier_Letter;

   --  General_Category=Other_Letter (Lo)

   Other_Letter_Set : Character_Set_Access := null;
   Other_Letter_Flag : aliased System.Once.Flag := 0;

   procedure Other_Letter_Init;
   procedure Other_Letter_Init is
   begin
      Other_Letter_Set := new Character_Set'(
         Length => UCD.General_Category.Lo_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Other_Letter_Set.Items,
         UCD.General_Category.Lo_Table_XXXXx1,
         UCD.General_Category.Lo_Table_XXXXx2,
         UCD.General_Category.Lo_Table_1XXXXx1,
         UCD.General_Category.Lo_Table_1XXXXx2,
         UCD.General_Category.Lo_Table_XXXXXXXXx1,
         UCD.General_Category.Lo_Table_XXXXXXXXx2);
   end Other_Letter_Init;

   function Other_Letter return not null access Character_Set is
   begin
      System.Once.Initialize (
         Other_Letter_Flag'Access,
         Other_Letter_Init'Access);
      return Other_Letter_Set;
   end Other_Letter;

   --  General_Category=Nonspacing_Mark (Mn)

   Nonspacing_Mark_Set : Character_Set_Access := null;
   Nonspacing_Mark_Flag : aliased System.Once.Flag := 0;

   procedure Nonspacing_Mark_Init;
   procedure Nonspacing_Mark_Init is
   begin
      Nonspacing_Mark_Set := new Character_Set'(
         Length => UCD.General_Category.Mn_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Nonspacing_Mark_Set.Items,
         UCD.General_Category.Mn_Table_XXXXx1,
         UCD.General_Category.Mn_Table_XXXXx2,
         UCD.General_Category.Mn_Table_1XXXXx1,
         UCD.General_Category.Mn_Table_1XXXXx2,
         UCD.General_Category.Mn_Table_XXXXXXXXx1,
         UCD.General_Category.Mn_Table_XXXXXXXXx2);
   end Nonspacing_Mark_Init;

   function Nonspacing_Mark return not null access Character_Set is
   begin
      System.Once.Initialize (
         Nonspacing_Mark_Flag'Access,
         Nonspacing_Mark_Init'Access);
      return Nonspacing_Mark_Set;
   end Nonspacing_Mark;

   --  General_Category=Enclosing_Mark (Me)

   Enclosing_Mark_Set : Character_Set_Access := null;
   Enclosing_Mark_Flag : aliased System.Once.Flag := 0;

   procedure Enclosing_Mark_Init;
   procedure Enclosing_Mark_Init is
   begin
      Enclosing_Mark_Set := new Character_Set'(
         Length => UCD.General_Category.Me_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Enclosing_Mark_Set.Items,
         UCD.General_Category.Me_Table_XXXXx1,
         UCD.General_Category.Me_Table_XXXXx2,
         UCD.General_Category.Me_Table_1XXXXx1,
         UCD.General_Category.Me_Table_1XXXXx2,
         UCD.General_Category.Me_Table_XXXXXXXXx1,
         UCD.General_Category.Me_Table_XXXXXXXXx2);
   end Enclosing_Mark_Init;

   function Enclosing_Mark return not null access Character_Set is
   begin
      System.Once.Initialize (
         Enclosing_Mark_Flag'Access,
         Enclosing_Mark_Init'Access);
      return Enclosing_Mark_Set;
   end Enclosing_Mark;

   --  General_Category=Spacing_Mark (Mc)

   Spacing_Mark_Set : Character_Set_Access := null;
   Spacing_Mark_Flag : aliased System.Once.Flag := 0;

   procedure Spacing_Mark_Init;
   procedure Spacing_Mark_Init is
   begin
      Spacing_Mark_Set := new Character_Set'(
         Length => UCD.General_Category.Mc_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Spacing_Mark_Set.Items,
         UCD.General_Category.Mc_Table_XXXXx1,
         UCD.General_Category.Mc_Table_XXXXx2,
         UCD.General_Category.Mc_Table_1XXXXx1,
         UCD.General_Category.Mc_Table_1XXXXx2,
         UCD.General_Category.Mc_Table_XXXXXXXXx1,
         UCD.General_Category.Mc_Table_XXXXXXXXx2);
   end Spacing_Mark_Init;

   function Spacing_Mark return not null access Character_Set is
   begin
      System.Once.Initialize (
         Spacing_Mark_Flag'Access,
         Spacing_Mark_Init'Access);
      return Spacing_Mark_Set;
   end Spacing_Mark;

   --  General_Category=Decimal_Number (Nd)

   Decimal_Number_Set : Character_Set_Access := null;
   Decimal_Number_Flag : aliased System.Once.Flag := 0;

   procedure Decimal_Number_Init;
   procedure Decimal_Number_Init is
   begin
      Decimal_Number_Set := new Character_Set'(
         Length => UCD.General_Category.Nd_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Decimal_Number_Set.Items,
         UCD.General_Category.Nd_Table_XXXXx1,
         UCD.General_Category.Nd_Table_XXXXx2,
         UCD.General_Category.Nd_Table_1XXXXx1,
         UCD.General_Category.Nd_Table_1XXXXx2,
         UCD.General_Category.Nd_Table_XXXXXXXXx1,
         UCD.General_Category.Nd_Table_XXXXXXXXx2);
   end Decimal_Number_Init;

   function Decimal_Number return not null access Character_Set is
   begin
      System.Once.Initialize (
         Decimal_Number_Flag'Access,
         Decimal_Number_Init'Access);
      return Decimal_Number_Set;
   end Decimal_Number;

   --  General_Category=Letter_Number (Nl)

   Letter_Number_Set : Character_Set_Access := null;
   Letter_Number_Flag : aliased System.Once.Flag := 0;

   procedure Letter_Number_Init;
   procedure Letter_Number_Init is
   begin
      Letter_Number_Set := new Character_Set'(
         Length => UCD.General_Category.Nl_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Letter_Number_Set.Items,
         UCD.General_Category.Nl_Table_XXXXx1,
         UCD.General_Category.Nl_Table_XXXXx2,
         UCD.General_Category.Nl_Table_1XXXXx1,
         UCD.General_Category.Nl_Table_1XXXXx2,
         UCD.General_Category.Nl_Table_XXXXXXXXx1,
         UCD.General_Category.Nl_Table_XXXXXXXXx2);
   end Letter_Number_Init;

   function Letter_Number return not null access Character_Set is
   begin
      System.Once.Initialize (
         Letter_Number_Flag'Access,
         Letter_Number_Init'Access);
      return Letter_Number_Set;
   end Letter_Number;

   --  General_Category=Other_Number (No)

   Other_Number_Set : Character_Set_Access := null;
   Other_Number_Flag : aliased System.Once.Flag := 0;

   procedure Other_Number_Init;
   procedure Other_Number_Init is
   begin
      Other_Number_Set := new Character_Set'(
         Length => UCD.General_Category.No_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Other_Number_Set.Items,
         UCD.General_Category.No_Table_XXXXx1,
         UCD.General_Category.No_Table_XXXXx2,
         UCD.General_Category.No_Table_1XXXXx1,
         UCD.General_Category.No_Table_1XXXXx2,
         UCD.General_Category.No_Table_XXXXXXXXx1,
         UCD.General_Category.No_Table_XXXXXXXXx2);
   end Other_Number_Init;

   function Other_Number return not null access Character_Set is
   begin
      System.Once.Initialize (
         Other_Number_Flag'Access,
         Other_Number_Init'Access);
      return Other_Number_Set;
   end Other_Number;

   --  General_Category=Space_Separator (Zs)

   Space_Separator_Set : Character_Set_Access := null;
   Space_Separator_Flag : aliased System.Once.Flag := 0;

   procedure Space_Separator_Init;
   procedure Space_Separator_Init is
   begin
      Space_Separator_Set := new Character_Set'(
         Length => UCD.General_Category.Zs_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Space_Separator_Set.Items,
         UCD.General_Category.Zs_Table_XXXXx1,
         UCD.General_Category.Zs_Table_XXXXx2,
         UCD.General_Category.Zs_Table_1XXXXx1,
         UCD.General_Category.Zs_Table_1XXXXx2,
         UCD.General_Category.Zs_Table_XXXXXXXXx1,
         UCD.General_Category.Zs_Table_XXXXXXXXx2);
   end Space_Separator_Init;

   function Space_Separator return not null access Character_Set is
   begin
      System.Once.Initialize (
         Space_Separator_Flag'Access,
         Space_Separator_Init'Access);
      return Space_Separator_Set;
   end Space_Separator;

   --  General_Category=Line_Separator (Zl)

   Line_Separator_Set : Character_Set_Access := null;
   Line_Separator_Flag : aliased System.Once.Flag := 0;

   procedure Line_Separator_Init;
   procedure Line_Separator_Init is
   begin
      Line_Separator_Set := new Character_Set'(
         Length => UCD.General_Category.Zl_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Line_Separator_Set.Items,
         UCD.General_Category.Zl_Table_XXXXx1,
         UCD.General_Category.Zl_Table_XXXXx2,
         UCD.General_Category.Zl_Table_1XXXXx1,
         UCD.General_Category.Zl_Table_1XXXXx2,
         UCD.General_Category.Zl_Table_XXXXXXXXx1,
         UCD.General_Category.Zl_Table_XXXXXXXXx2);
   end Line_Separator_Init;

   function Line_Separator return not null access Character_Set is
   begin
      System.Once.Initialize (
         Line_Separator_Flag'Access,
         Line_Separator_Init'Access);
      return Line_Separator_Set;
   end Line_Separator;

   --  General_Category=Paragraph_Separator (Zp)

   Paragraph_Separator_Set : Character_Set_Access := null;
   Paragraph_Separator_Flag : aliased System.Once.Flag := 0;

   procedure Paragraph_Separator_Init;
   procedure Paragraph_Separator_Init is
   begin
      Paragraph_Separator_Set := new Character_Set'(
         Length => UCD.General_Category.Zp_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Paragraph_Separator_Set.Items,
         UCD.General_Category.Zp_Table_XXXXx1,
         UCD.General_Category.Zp_Table_XXXXx2,
         UCD.General_Category.Zp_Table_1XXXXx1,
         UCD.General_Category.Zp_Table_1XXXXx2,
         UCD.General_Category.Zp_Table_XXXXXXXXx1,
         UCD.General_Category.Zp_Table_XXXXXXXXx2);
   end Paragraph_Separator_Init;

   function Paragraph_Separator return not null access Character_Set is
   begin
      System.Once.Initialize (
         Paragraph_Separator_Flag'Access,
         Paragraph_Separator_Init'Access);
      return Paragraph_Separator_Set;
   end Paragraph_Separator;

   --  General_Category=Control (Cc)

   Control_Set : Character_Set_Access := null;
   Control_Flag : aliased System.Once.Flag := 0;

   procedure Control_Init;
   procedure Control_Init is
   begin
      Control_Set := new Character_Set'(
         Length => UCD.General_Category.Cc_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Control_Set.Items,
         UCD.General_Category.Cc_Table_XXXXx1,
         UCD.General_Category.Cc_Table_XXXXx2,
         UCD.General_Category.Cc_Table_1XXXXx1,
         UCD.General_Category.Cc_Table_1XXXXx2,
         UCD.General_Category.Cc_Table_XXXXXXXXx1,
         UCD.General_Category.Cc_Table_XXXXXXXXx2);
   end Control_Init;

   function Control return not null access Character_Set is
   begin
      System.Once.Initialize (
         Control_Flag'Access,
         Control_Init'Access);
      return Control_Set;
   end Control;

   --  General_Category=Format (Cf)

   Format_Set : Character_Set_Access := null;
   Format_Flag : aliased System.Once.Flag := 0;

   procedure Format_Init;
   procedure Format_Init is
   begin
      Format_Set := new Character_Set'(
         Length => UCD.General_Category.Cf_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Format_Set.Items,
         UCD.General_Category.Cf_Table_XXXXx1,
         UCD.General_Category.Cf_Table_XXXXx2,
         UCD.General_Category.Cf_Table_1XXXXx1,
         UCD.General_Category.Cf_Table_1XXXXx2,
         UCD.General_Category.Cf_Table_XXXXXXXXx1,
         UCD.General_Category.Cf_Table_XXXXXXXXx2);
   end Format_Init;

   function Format return not null access Character_Set is
   begin
      System.Once.Initialize (
         Format_Flag'Access,
         Format_Init'Access);
      return Format_Set;
   end Format;

   --  General_Category=Private_Use (Co)

   Private_Use_Set : Character_Set_Access := null;
   Private_Use_Flag : aliased System.Once.Flag := 0;

   procedure Private_Use_Init;
   procedure Private_Use_Init is
   begin
      Private_Use_Set := new Character_Set'(
         Length => UCD.General_Category.Co_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Private_Use_Set.Items,
         UCD.General_Category.Co_Table_XXXXx1,
         UCD.General_Category.Co_Table_XXXXx2,
         UCD.General_Category.Co_Table_1XXXXx1,
         UCD.General_Category.Co_Table_1XXXXx2,
         UCD.General_Category.Co_Table_XXXXXXXXx1,
         UCD.General_Category.Co_Table_XXXXXXXXx2);
   end Private_Use_Init;

   function Private_Use return not null access Character_Set is
   begin
      System.Once.Initialize (
         Private_Use_Flag'Access,
         Private_Use_Init'Access);
      return Private_Use_Set;
   end Private_Use;

   --  General_Category=Surrogate (Cs)

   Surrogate_Set : Character_Set_Access := null;
   Surrogate_Flag : aliased System.Once.Flag := 0;

   procedure Surrogate_Init;
   procedure Surrogate_Init is
   begin
      Surrogate_Set := new Character_Set'(
         Length => UCD.General_Category.Cs_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Surrogate_Set.Items,
         UCD.General_Category.Cs_Table_XXXXx1,
         UCD.General_Category.Cs_Table_XXXXx2,
         UCD.General_Category.Cs_Table_1XXXXx1,
         UCD.General_Category.Cs_Table_1XXXXx2,
         UCD.General_Category.Cs_Table_XXXXXXXXx1,
         UCD.General_Category.Cs_Table_XXXXXXXXx2);
   end Surrogate_Init;

   function Surrogate return not null access Character_Set is
   begin
      System.Once.Initialize (
         Surrogate_Flag'Access,
         Surrogate_Init'Access);
      return Surrogate_Set;
   end Surrogate;

   --  General_Category=Dash_Punctuation (Pd)

   Dash_Punctuation_Set : Character_Set_Access := null;
   Dash_Punctuation_Flag : aliased System.Once.Flag := 0;

   procedure Dash_Punctuation_Init;
   procedure Dash_Punctuation_Init is
   begin
      Dash_Punctuation_Set := new Character_Set'(
         Length => UCD.General_Category.Pd_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Dash_Punctuation_Set.Items,
         UCD.General_Category.Pd_Table_XXXXx1,
         UCD.General_Category.Pd_Table_XXXXx2,
         UCD.General_Category.Pd_Table_1XXXXx1,
         UCD.General_Category.Pd_Table_1XXXXx2,
         UCD.General_Category.Pd_Table_XXXXXXXXx1,
         UCD.General_Category.Pd_Table_XXXXXXXXx2);
   end Dash_Punctuation_Init;

   function Dash_Punctuation return not null access Character_Set is
   begin
      System.Once.Initialize (
         Dash_Punctuation_Flag'Access,
         Dash_Punctuation_Init'Access);
      return Dash_Punctuation_Set;
   end Dash_Punctuation;

   --  General_Category=Open_Punctuation (Ps)

   Open_Punctuation_Set : Character_Set_Access := null;
   Open_Punctuation_Flag : aliased System.Once.Flag := 0;

   procedure Open_Punctuation_Init;
   procedure Open_Punctuation_Init is
   begin
      Open_Punctuation_Set := new Character_Set'(
         Length => UCD.General_Category.Ps_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Open_Punctuation_Set.Items,
         UCD.General_Category.Ps_Table_XXXXx1,
         UCD.General_Category.Ps_Table_XXXXx2,
         UCD.General_Category.Ps_Table_1XXXXx1,
         UCD.General_Category.Ps_Table_1XXXXx2,
         UCD.General_Category.Ps_Table_XXXXXXXXx1,
         UCD.General_Category.Ps_Table_XXXXXXXXx2);
   end Open_Punctuation_Init;

   function Open_Punctuation return not null access Character_Set is
   begin
      System.Once.Initialize (
         Open_Punctuation_Flag'Access,
         Open_Punctuation_Init'Access);
      return Open_Punctuation_Set;
   end Open_Punctuation;

   --  General_Category=Close_Punctuation (Pe)

   Close_Punctuation_Set : Character_Set_Access := null;
   Close_Punctuation_Flag : aliased System.Once.Flag := 0;

   procedure Close_Punctuation_Init;
   procedure Close_Punctuation_Init is
   begin
      Close_Punctuation_Set := new Character_Set'(
         Length => UCD.General_Category.Pe_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Close_Punctuation_Set.Items,
         UCD.General_Category.Pe_Table_XXXXx1,
         UCD.General_Category.Pe_Table_XXXXx2,
         UCD.General_Category.Pe_Table_1XXXXx1,
         UCD.General_Category.Pe_Table_1XXXXx2,
         UCD.General_Category.Pe_Table_XXXXXXXXx1,
         UCD.General_Category.Pe_Table_XXXXXXXXx2);
   end Close_Punctuation_Init;

   function Close_Punctuation return not null access Character_Set is
   begin
      System.Once.Initialize (
         Close_Punctuation_Flag'Access,
         Close_Punctuation_Init'Access);
      return Close_Punctuation_Set;
   end Close_Punctuation;

   --  General_Category=Connector_Punctuation (Pc)

   Connector_Punctuation_Set : Character_Set_Access := null;
   Connector_Punctuation_Flag : aliased System.Once.Flag := 0;

   procedure Connector_Punctuation_Init;
   procedure Connector_Punctuation_Init is
   begin
      Connector_Punctuation_Set := new Character_Set'(
         Length => UCD.General_Category.Pc_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Connector_Punctuation_Set.Items,
         UCD.General_Category.Pc_Table_XXXXx1,
         UCD.General_Category.Pc_Table_XXXXx2,
         UCD.General_Category.Pc_Table_1XXXXx1,
         UCD.General_Category.Pc_Table_1XXXXx2,
         UCD.General_Category.Pc_Table_XXXXXXXXx1,
         UCD.General_Category.Pc_Table_XXXXXXXXx2);
   end Connector_Punctuation_Init;

   function Connector_Punctuation return not null access Character_Set is
   begin
      System.Once.Initialize (
         Connector_Punctuation_Flag'Access,
         Connector_Punctuation_Init'Access);
      return Connector_Punctuation_Set;
   end Connector_Punctuation;

   --  General_Category=Other_Punctuation (Po)

   Other_Punctuation_Set : Character_Set_Access := null;
   Other_Punctuation_Flag : aliased System.Once.Flag := 0;

   procedure Other_Punctuation_Init;
   procedure Other_Punctuation_Init is
   begin
      Other_Punctuation_Set := new Character_Set'(
         Length => UCD.General_Category.Po_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Other_Punctuation_Set.Items,
         UCD.General_Category.Po_Table_XXXXx1,
         UCD.General_Category.Po_Table_XXXXx2,
         UCD.General_Category.Po_Table_1XXXXx1,
         UCD.General_Category.Po_Table_1XXXXx2,
         UCD.General_Category.Po_Table_XXXXXXXXx1,
         UCD.General_Category.Po_Table_XXXXXXXXx2);
   end Other_Punctuation_Init;

   function Other_Punctuation return not null access Character_Set is
   begin
      System.Once.Initialize (
         Other_Punctuation_Flag'Access,
         Other_Punctuation_Init'Access);
      return Other_Punctuation_Set;
   end Other_Punctuation;

   --  General_Category=Math_Symbol (Sm)

   Math_Symbol_Set : Character_Set_Access := null;
   Math_Symbol_Flag : aliased System.Once.Flag := 0;

   procedure Math_Symbol_Init;
   procedure Math_Symbol_Init is
   begin
      Math_Symbol_Set := new Character_Set'(
         Length => UCD.General_Category.Sm_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Math_Symbol_Set.Items,
         UCD.General_Category.Sm_Table_XXXXx1,
         UCD.General_Category.Sm_Table_XXXXx2,
         UCD.General_Category.Sm_Table_1XXXXx1,
         UCD.General_Category.Sm_Table_1XXXXx2,
         UCD.General_Category.Sm_Table_XXXXXXXXx1,
         UCD.General_Category.Sm_Table_XXXXXXXXx2);
   end Math_Symbol_Init;

   function Math_Symbol return not null access Character_Set is
   begin
      System.Once.Initialize (
         Math_Symbol_Flag'Access,
         Math_Symbol_Init'Access);
      return Math_Symbol_Set;
   end Math_Symbol;

   --  General_Category=Currency_Symbol (Sc)

   Currency_Symbol_Set : Character_Set_Access := null;
   Currency_Symbol_Flag : aliased System.Once.Flag := 0;

   procedure Currency_Symbol_Init;
   procedure Currency_Symbol_Init is
   begin
      Currency_Symbol_Set := new Character_Set'(
         Length => UCD.General_Category.Sc_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Currency_Symbol_Set.Items,
         UCD.General_Category.Sc_Table_XXXXx1,
         UCD.General_Category.Sc_Table_XXXXx2,
         UCD.General_Category.Sc_Table_1XXXXx1,
         UCD.General_Category.Sc_Table_1XXXXx2,
         UCD.General_Category.Sc_Table_XXXXXXXXx1,
         UCD.General_Category.Sc_Table_XXXXXXXXx2);
   end Currency_Symbol_Init;

   function Currency_Symbol return not null access Character_Set is
   begin
      System.Once.Initialize (
         Currency_Symbol_Flag'Access,
         Currency_Symbol_Init'Access);
      return Currency_Symbol_Set;
   end Currency_Symbol;

   --  General_Category=Modifier_Symbol (Sk)

   Modifier_Symbol_Set : Character_Set_Access := null;
   Modifier_Symbol_Flag : aliased System.Once.Flag := 0;

   procedure Modifier_Symbol_Init;
   procedure Modifier_Symbol_Init is
   begin
      Modifier_Symbol_Set := new Character_Set'(
         Length => UCD.General_Category.Sk_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Modifier_Symbol_Set.Items,
         UCD.General_Category.Sk_Table_XXXXx1,
         UCD.General_Category.Sk_Table_XXXXx2,
         UCD.General_Category.Sk_Table_1XXXXx1,
         UCD.General_Category.Sk_Table_1XXXXx2,
         UCD.General_Category.Sk_Table_XXXXXXXXx1,
         UCD.General_Category.Sk_Table_XXXXXXXXx2);
   end Modifier_Symbol_Init;

   function Modifier_Symbol return not null access Character_Set is
   begin
      System.Once.Initialize (
         Modifier_Symbol_Flag'Access,
         Modifier_Symbol_Init'Access);
      return Modifier_Symbol_Set;
   end Modifier_Symbol;

   --  General_Category=Other_Symbol (So)

   Other_Symbol_Set : Character_Set_Access := null;
   Other_Symbol_Flag : aliased System.Once.Flag := 0;

   procedure Other_Symbol_Init;
   procedure Other_Symbol_Init is
   begin
      Other_Symbol_Set := new Character_Set'(
         Length => UCD.General_Category.So_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Other_Symbol_Set.Items,
         UCD.General_Category.So_Table_XXXXx1,
         UCD.General_Category.So_Table_XXXXx2,
         UCD.General_Category.So_Table_1XXXXx1,
         UCD.General_Category.So_Table_1XXXXx2,
         UCD.General_Category.So_Table_XXXXXXXXx1,
         UCD.General_Category.So_Table_XXXXXXXXx2);
   end Other_Symbol_Init;

   function Other_Symbol return not null access Character_Set is
   begin
      System.Once.Initialize (
         Other_Symbol_Flag'Access,
         Other_Symbol_Init'Access);
      return Other_Symbol_Set;
   end Other_Symbol;

   --  General_Category=Initial_Punctuation (Pi)

   Initial_Punctuation_Set : Character_Set_Access := null;
   Initial_Punctuation_Flag : aliased System.Once.Flag := 0;

   procedure Initial_Punctuation_Init;
   procedure Initial_Punctuation_Init is
   begin
      Initial_Punctuation_Set := new Character_Set'(
         Length => UCD.General_Category.Pi_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Initial_Punctuation_Set.Items,
         UCD.General_Category.Pi_Table_XXXXx1,
         UCD.General_Category.Pi_Table_XXXXx2,
         UCD.General_Category.Pi_Table_1XXXXx1,
         UCD.General_Category.Pi_Table_1XXXXx2,
         UCD.General_Category.Pi_Table_XXXXXXXXx1,
         UCD.General_Category.Pi_Table_XXXXXXXXx2);
   end Initial_Punctuation_Init;

   function Initial_Punctuation return not null access Character_Set is
   begin
      System.Once.Initialize (
         Initial_Punctuation_Flag'Access,
         Initial_Punctuation_Init'Access);
      return Initial_Punctuation_Set;
   end Initial_Punctuation;

   --  General_Category=Final_Punctuation (Pf)

   Final_Punctuation_Set : Character_Set_Access := null;
   Final_Punctuation_Flag : aliased System.Once.Flag := 0;

   procedure Final_Punctuation_Init;
   procedure Final_Punctuation_Init is
   begin
      Final_Punctuation_Set := new Character_Set'(
         Length => UCD.General_Category.Pf_Range_Length,
         Reference_Count => System.Reference_Counting.Static,
         Items => <>);
      Fill (
         Final_Punctuation_Set.Items,
         UCD.General_Category.Pf_Table_XXXXx1,
         UCD.General_Category.Pf_Table_XXXXx2,
         UCD.General_Category.Pf_Table_1XXXXx1,
         UCD.General_Category.Pf_Table_1XXXXx2,
         UCD.General_Category.Pf_Table_XXXXXXXXx1,
         UCD.General_Category.Pf_Table_XXXXXXXXx2);
   end Final_Punctuation_Init;

   function Final_Punctuation return not null access Character_Set is
   begin
      System.Once.Initialize (
         Final_Punctuation_Flag'Access,
         Final_Punctuation_Init'Access);
      return Final_Punctuation_Set;
   end Final_Punctuation;

end Ada.Characters.Inside.Sets.General_Category;
