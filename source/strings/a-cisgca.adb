with Ada.UCD.General_Category;
package body Ada.Characters.Inside.Sets.General_Category is
   use type Interfaces.Integer_32;

   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.General_Category.Table_Type_2x1);
   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.General_Category.Table_Type_2x1) is
   begin
      pragma Assert (Table'Length = To'Length);
      for I in Table'Range loop
         declare
            T : Character_Range renames To (To'First - Table'First + I);
         begin
            T.Low := Wide_Wide_Character'Val (Table (I));
            T.High := Wide_Wide_Character'Val (Table (I));
         end;
      end loop;
   end Fill;

   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.General_Category.Table_Type_4x1);
   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.General_Category.Table_Type_4x1) is
   begin
      pragma Assert (Table'Length = To'Length);
      for I in Table'Range loop
         declare
            T : Character_Range renames To (To'First - Table'First + I);
         begin
            T.Low := Wide_Wide_Character'Val (Table (I));
            T.High := Wide_Wide_Character'Val (Table (I));
         end;
      end loop;
   end Fill;

   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.General_Category.Table_Type_2x2);
   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.General_Category.Table_Type_2x2) is
   begin
      pragma Assert (Table'Length = To'Length);
      for I in Table'Range loop
         declare
            T : Character_Range renames To (To'First - Table'First + I);
         begin
            T.Low := Wide_Wide_Character'Val (Table (I).Low);
            T.High := Wide_Wide_Character'Val (Table (I).High);
         end;
      end loop;
   end Fill;

   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.General_Category.Table_Type_4x2);
   procedure Fill (
      To : out Character_Ranges;
      Table : UCD.General_Category.Table_Type_4x2) is
   begin
      pragma Assert (Table'Length = To'Length);
      for I in Table'Range loop
         declare
            T : Character_Range renames To (To'First - Table'First + I);
         begin
            T.Low := Wide_Wide_Character'Val (Table (I).Low);
            T.High := Wide_Wide_Character'Val (Table (I).High);
         end;
      end loop;
   end Fill;

   procedure Fill (
      To : in out Character_Ranges;
      Table_2x1 : UCD.General_Category.Table_Type_2x1;
      Table_2x2 : UCD.General_Category.Table_Type_2x2);
   procedure Fill (
      To : in out Character_Ranges;
      Table_2x1 : UCD.General_Category.Table_Type_2x1;
      Table_2x2 : UCD.General_Category.Table_Type_2x2)
   is
      T2x1 : Character_Ranges (Table_2x1'Range);
      T2x2 : Character_Ranges (Table_2x2'Range);
      Last : Natural;
   begin
      Fill (T2x1, Table_2x1);
      Fill (T2x2, Table_2x2);
      Merge (To, Last, T2x1, T2x2);
      pragma Assert (Last = To'Last);
   end Fill;

   procedure Fill (
      To : in out Character_Ranges;
      Table_2x2 : UCD.General_Category.Table_Type_2x2;
      Table_4x2 : UCD.General_Category.Table_Type_4x2);
   procedure Fill (
      To : in out Character_Ranges;
      Table_2x2 : UCD.General_Category.Table_Type_2x2;
      Table_4x2 : UCD.General_Category.Table_Type_4x2)
   is
      T2x2 : Character_Ranges (Table_2x2'Range);
      T4x2 : Character_Ranges (Table_4x2'Range);
      Last : Natural;
   begin
      Fill (T2x2, Table_2x2);
      Fill (T4x2, Table_4x2);
      Merge (To, Last, T2x2, T4x2);
      pragma Assert (Last = To'Last);
   end Fill;

   procedure Fill (
      To : in out Character_Ranges;
      Table_2x1 : UCD.General_Category.Table_Type_2x1;
      Table_4x1 : UCD.General_Category.Table_Type_4x1;
      Table_2x2 : UCD.General_Category.Table_Type_2x2);
   procedure Fill (
      To : in out Character_Ranges;
      Table_2x1 : UCD.General_Category.Table_Type_2x1;
      Table_4x1 : UCD.General_Category.Table_Type_4x1;
      Table_2x2 : UCD.General_Category.Table_Type_2x2)
   is
      T2x1 : Character_Ranges (Table_2x1'Range);
      T4x1 : Character_Ranges (Table_4x1'Range);
      TAx1 : Character_Ranges (1 .. Table_2x1'Length + Table_4x1'Length);
      T2x2 : Character_Ranges (Table_2x2'Range);
      Last : Natural;
   begin
      Fill (T2x1, Table_2x1);
      Fill (T4x1, Table_4x1);
      Merge (TAx1, Last, T2x1, T4x1);
      pragma Assert (Last = TAx1'Last);
      Fill (T2x2, Table_2x2);
      Merge (To, Last, TAx1, T2x2);
      pragma Assert (Last = To'Last);
   end Fill;

   procedure Fill (
      To : in out Character_Ranges;
      Table_2x1 : UCD.General_Category.Table_Type_2x1;
      Table_4x1 : UCD.General_Category.Table_Type_4x1;
      Table_2x2 : UCD.General_Category.Table_Type_2x2;
      Table_4x2 : UCD.General_Category.Table_Type_4x2);
   procedure Fill (
      To : in out Character_Ranges;
      Table_2x1 : UCD.General_Category.Table_Type_2x1;
      Table_4x1 : UCD.General_Category.Table_Type_4x1;
      Table_2x2 : UCD.General_Category.Table_Type_2x2;
      Table_4x2 : UCD.General_Category.Table_Type_4x2)
   is
      T2x1 : Character_Ranges (Table_2x1'Range);
      T4x1 : Character_Ranges (Table_4x1'Range);
      TAx1 : Character_Ranges (1 .. Table_2x1'Length + Table_4x1'Length);
      T2x2 : Character_Ranges (Table_2x2'Range);
      T4x2 : Character_Ranges (Table_4x2'Range);
      TAx2 : Character_Ranges (1 .. Table_2x2'Length + Table_4x2'Length);
      Last : Natural;
   begin
      Fill (T2x1, Table_2x1);
      Fill (T4x1, Table_4x1);
      Merge (TAx1, Last, T2x1, T4x1);
      pragma Assert (Last = TAx1'Last);
      Fill (T2x2, Table_2x2);
      Fill (T4x2, Table_4x2);
      Merge (TAx2, Last, T2x2, T4x2);
      pragma Assert (Last = TAx2'Last);
      Merge (To, Last, TAx1, TAx2);
      pragma Assert (Last = To'Last);
   end Fill;

   type Character_Set_Access is access Character_Set;

   --  General_Category=Unassigned (Cn)

   Unassigned_Set : Character_Set_Access := null;

   function Unassigned return not null access Character_Set is
   begin
      if Unassigned_Set = null then
         Unassigned_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Cn_Table_2x1'Length +
               UCD.General_Category.Cn_Table_4x1'Length +
               UCD.General_Category.Cn_Table_2x2'Length +
               UCD.General_Category.Cn_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Unassigned_Set.Items,
            UCD.General_Category.Cn_Table_2x1,
            UCD.General_Category.Cn_Table_4x1,
            UCD.General_Category.Cn_Table_2x2,
            UCD.General_Category.Cn_Table_4x2);
      end if;
      return Unassigned_Set;
   end Unassigned;

   All_Unassigned_Set : Character_Set_Access := null;

   function All_Unassigned return not null access Character_Set is
   begin
      if All_Unassigned_Set = null then
         declare
            UA : constant not null access Character_Set := Unassigned;
         begin
            All_Unassigned_Set := new Character_Set'(
               Length => UA.Length,
               Reference_Count => -1, --  constant
               Items => UA.Items);
         end;
         pragma Assert (
            All_Unassigned_Set.Items (All_Unassigned_Set.Items'Last).High =
            Character_Type'Val (16#10FFFF#));
         All_Unassigned_Set.Items (All_Unassigned_Set.Items'Last).High :=
            Character_Type'Last;
      end if;
      return All_Unassigned_Set;
   end All_Unassigned;

   --  General_Category=Uppercase_Letter (Lu)

   Uppercase_Letter_Set : Character_Set_Access := null;

   function Uppercase_Letter return not null access Character_Set is
   begin
      if Uppercase_Letter_Set = null then
         Uppercase_Letter_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Lu_Table_2x1'Length +
               UCD.General_Category.Lu_Table_4x1'Length +
               UCD.General_Category.Lu_Table_2x2'Length +
               UCD.General_Category.Lu_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Uppercase_Letter_Set.Items,
            UCD.General_Category.Lu_Table_2x1,
            UCD.General_Category.Lu_Table_4x1,
            UCD.General_Category.Lu_Table_2x2,
            UCD.General_Category.Lu_Table_4x2);
      end if;
      return Uppercase_Letter_Set;
   end Uppercase_Letter;

   --  General_Category=Lowercase_Letter (Ll)

   Lowercase_Letter_Set : Character_Set_Access := null;

   function Lowercase_Letter return not null access Character_Set is
   begin
      if Lowercase_Letter_Set = null then
         Lowercase_Letter_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Ll_Table_2x1'Length +
               UCD.General_Category.Ll_Table_4x1'Length +
               UCD.General_Category.Ll_Table_2x2'Length +
               UCD.General_Category.Ll_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Lowercase_Letter_Set.Items,
            UCD.General_Category.Ll_Table_2x1,
            UCD.General_Category.Ll_Table_4x1,
            UCD.General_Category.Ll_Table_2x2,
            UCD.General_Category.Ll_Table_4x2);
      end if;
      return Lowercase_Letter_Set;
   end Lowercase_Letter;

   --  General_Category=Titlecase_Letter (Lt)

   Titlecase_Letter_Set : Character_Set_Access := null;

   function Titlecase_Letter return not null access Character_Set is
   begin
      if Titlecase_Letter_Set = null then
         Titlecase_Letter_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Lt_Table_2x1'Length +
               UCD.General_Category.Lt_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Titlecase_Letter_Set.Items,
            UCD.General_Category.Lt_Table_2x1,
            UCD.General_Category.Lt_Table_2x2);
      end if;
      return Titlecase_Letter_Set;
   end Titlecase_Letter;

   --  General_Category=Modifier_Letter (Lm)

   Modifier_Letter_Set : Character_Set_Access := null;

   function Modifier_Letter return not null access Character_Set is
   begin
      if Modifier_Letter_Set = null then
         Modifier_Letter_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Lm_Table_2x1'Length +
               UCD.General_Category.Lm_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Modifier_Letter_Set.Items,
            UCD.General_Category.Lm_Table_2x1,
            UCD.General_Category.Lm_Table_2x2);
      end if;
      return Modifier_Letter_Set;
   end Modifier_Letter;

   --  General_Category=Other_Letter (Lo)

   Other_Letter_Set : Character_Set_Access := null;

   function Other_Letter return not null access Character_Set is
   begin
      if Other_Letter_Set = null then
         Other_Letter_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Lo_Table_2x1'Length +
               UCD.General_Category.Lo_Table_4x1'Length +
               UCD.General_Category.Lo_Table_2x2'Length +
               UCD.General_Category.Lo_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Other_Letter_Set.Items,
            UCD.General_Category.Lo_Table_2x1,
            UCD.General_Category.Lo_Table_4x1,
            UCD.General_Category.Lo_Table_2x2,
            UCD.General_Category.Lo_Table_4x2);
      end if;
      return Other_Letter_Set;
   end Other_Letter;

   --  General_Category=Nonspacing_Mark (Mn)

   Nonspacing_Mark_Set : Character_Set_Access := null;

   function Nonspacing_Mark return not null access Character_Set is
   begin
      if Nonspacing_Mark_Set = null then
         Nonspacing_Mark_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Mn_Table_2x1'Length +
               UCD.General_Category.Mn_Table_4x1'Length +
               UCD.General_Category.Mn_Table_2x2'Length +
               UCD.General_Category.Mn_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Nonspacing_Mark_Set.Items,
            UCD.General_Category.Mn_Table_2x1,
            UCD.General_Category.Mn_Table_4x1,
            UCD.General_Category.Mn_Table_2x2,
            UCD.General_Category.Mn_Table_4x2);
      end if;
      return Nonspacing_Mark_Set;
   end Nonspacing_Mark;

   --  General_Category=Enclosing_Mark (Me)

   Enclosing_Mark_Set : Character_Set_Access := null;

   function Enclosing_Mark return not null access Character_Set is
   begin
      if Enclosing_Mark_Set = null then
         Enclosing_Mark_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Me_Table_2x1'Length +
               UCD.General_Category.Me_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Enclosing_Mark_Set.Items,
            UCD.General_Category.Me_Table_2x1,
            UCD.General_Category.Me_Table_2x2);
      end if;
      return Enclosing_Mark_Set;
   end Enclosing_Mark;

   --  General_Category=Spacing_Mark (Mc)

   Spacing_Mark_Set : Character_Set_Access := null;

   function Spacing_Mark return not null access Character_Set is
   begin
      if Spacing_Mark_Set = null then
         Spacing_Mark_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Mc_Table_2x1'Length +
               UCD.General_Category.Mc_Table_4x1'Length +
               UCD.General_Category.Mc_Table_2x2'Length +
               UCD.General_Category.Mc_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Spacing_Mark_Set.Items,
            UCD.General_Category.Mc_Table_2x1,
            UCD.General_Category.Mc_Table_4x1,
            UCD.General_Category.Mc_Table_2x2,
            UCD.General_Category.Mc_Table_4x2);
      end if;
      return Spacing_Mark_Set;
   end Spacing_Mark;

   --  General_Category=Decimal_Number (Nd)

   Decimal_Number_Set : Character_Set_Access := null;

   function Decimal_Number return not null access Character_Set is
   begin
      if Decimal_Number_Set = null then
         Decimal_Number_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Nd_Table_2x2'Length +
               UCD.General_Category.Nd_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Decimal_Number_Set.Items,
            UCD.General_Category.Nd_Table_2x2,
            UCD.General_Category.Nd_Table_4x2);
      end if;
      return Decimal_Number_Set;
   end Decimal_Number;

   --  General_Category=Letter_Number (Nl)

   Letter_Number_Set : Character_Set_Access := null;

   function Letter_Number return not null access Character_Set is
   begin
      if Letter_Number_Set = null then
         Letter_Number_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Nl_Table_2x1'Length +
               UCD.General_Category.Nl_Table_4x1'Length +
               UCD.General_Category.Nl_Table_2x2'Length +
               UCD.General_Category.Nl_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Letter_Number_Set.Items,
            UCD.General_Category.Nl_Table_2x1,
            UCD.General_Category.Nl_Table_4x1,
            UCD.General_Category.Nl_Table_2x2,
            UCD.General_Category.Nl_Table_4x2);
      end if;
      return Letter_Number_Set;
   end Letter_Number;

   --  General_Category=Other_Number (No)

   Other_Number_Set : Character_Set_Access := null;

   function Other_Number return not null access Character_Set is
   begin
      if Other_Number_Set = null then
         Other_Number_Set := new Character_Set'(
            Length =>
               UCD.General_Category.No_Table_2x1'Length +
               UCD.General_Category.No_Table_4x1'Length +
               UCD.General_Category.No_Table_2x2'Length +
               UCD.General_Category.No_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Other_Number_Set.Items,
            UCD.General_Category.No_Table_2x1,
            UCD.General_Category.No_Table_4x1,
            UCD.General_Category.No_Table_2x2,
            UCD.General_Category.No_Table_4x2);
      end if;
      return Other_Number_Set;
   end Other_Number;

   --  General_Category=Space_Separator (Zs)

   Space_Separator_Set : Character_Set_Access := null;

   function Space_Separator return not null access Character_Set is
   begin
      if Space_Separator_Set = null then
         Space_Separator_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Zs_Table_2x1'Length +
               UCD.General_Category.Zs_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Space_Separator_Set.Items,
            UCD.General_Category.Zs_Table_2x1,
            UCD.General_Category.Zs_Table_2x2);
      end if;
      return Space_Separator_Set;
   end Space_Separator;

   --  General_Category=Line_Separator (Zl)

   Line_Separator_Set : Character_Set_Access := null;

   function Line_Separator return not null access Character_Set is
   begin
      if Line_Separator_Set = null then
         Line_Separator_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Zl_Table_2x1'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Line_Separator_Set.Items,
            UCD.General_Category.Zl_Table_2x1);
      end if;
      return Line_Separator_Set;
   end Line_Separator;

   --  General_Category=Paragraph_Separator (Zp)

   Paragraph_Separator_Set : Character_Set_Access := null;

   function Paragraph_Separator return not null access Character_Set is
   begin
      if Paragraph_Separator_Set = null then
         Paragraph_Separator_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Zp_Table_2x1'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Paragraph_Separator_Set.Items,
            UCD.General_Category.Zp_Table_2x1);
      end if;
      return Paragraph_Separator_Set;
   end Paragraph_Separator;

   --  General_Category=Control (Cc)

   Control_Set : Character_Set_Access := null;

   function Control return not null access Character_Set is
   begin
      if Control_Set = null then
         Control_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Cc_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Control_Set.Items,
            UCD.General_Category.Cc_Table_2x2);
      end if;
      return Control_Set;
   end Control;

   --  General_Category=Format (Cf)

   Format_Set : Character_Set_Access := null;

   function Format return not null access Character_Set is
   begin
      if Format_Set = null then
         Format_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Cf_Table_2x1'Length +
               UCD.General_Category.Cf_Table_4x1'Length +
               UCD.General_Category.Cf_Table_2x2'Length +
               UCD.General_Category.Cf_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Format_Set.Items,
            UCD.General_Category.Cf_Table_2x1,
            UCD.General_Category.Cf_Table_4x1,
            UCD.General_Category.Cf_Table_2x2,
            UCD.General_Category.Cf_Table_4x2);
      end if;
      return Format_Set;
   end Format;

   --  General_Category=Private_Use (Co)

   Private_Use_Set : Character_Set_Access := null;

   function Private_Use return not null access Character_Set is
   begin
      if Private_Use_Set = null then
         Private_Use_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Co_Table_2x2'Length +
               UCD.General_Category.Co_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Private_Use_Set.Items,
            UCD.General_Category.Co_Table_2x2,
            UCD.General_Category.Co_Table_4x2);
      end if;
      return Private_Use_Set;
   end Private_Use;

   --  General_Category=Surrogate (Cs)

   Surrogate_Set : Character_Set_Access := null;

   function Surrogate return not null access Character_Set is
   begin
      if Surrogate_Set = null then
         Surrogate_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Cs_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Surrogate_Set.Items,
            UCD.General_Category.Cs_Table_2x2);
      end if;
      return Surrogate_Set;
   end Surrogate;

   --  General_Category=Dash_Punctuation (Pd)

   Dash_Punctuation_Set : Character_Set_Access := null;

   function Dash_Punctuation return not null access Character_Set is
   begin
      if Dash_Punctuation_Set = null then
         Dash_Punctuation_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Pd_Table_2x1'Length +
               UCD.General_Category.Pd_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Dash_Punctuation_Set.Items,
            UCD.General_Category.Pd_Table_2x1,
            UCD.General_Category.Pd_Table_2x2);
      end if;
      return Dash_Punctuation_Set;
   end Dash_Punctuation;

   --  General_Category=Open_Punctuation (Ps)

   Open_Punctuation_Set : Character_Set_Access := null;

   function Open_Punctuation return not null access Character_Set is
   begin
      if Open_Punctuation_Set = null then
         Open_Punctuation_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Ps_Table_2x1'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Open_Punctuation_Set.Items,
            UCD.General_Category.Ps_Table_2x1);
      end if;
      return Open_Punctuation_Set;
   end Open_Punctuation;

   --  General_Category=Close_Punctuation (Pe)

   Close_Punctuation_Set : Character_Set_Access := null;

   function Close_Punctuation return not null access Character_Set is
   begin
      if Close_Punctuation_Set = null then
         Close_Punctuation_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Pe_Table_2x1'Length +
               UCD.General_Category.Pe_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Close_Punctuation_Set.Items,
            UCD.General_Category.Pe_Table_2x1,
            UCD.General_Category.Pe_Table_2x2);
      end if;
      return Close_Punctuation_Set;
   end Close_Punctuation;

   --  General_Category=Connector_Punctuation (Pc)

   Connector_Punctuation_Set : Character_Set_Access := null;

   function Connector_Punctuation return not null access Character_Set is
   begin
      if Connector_Punctuation_Set = null then
         Connector_Punctuation_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Pc_Table_2x1'Length +
               UCD.General_Category.Pc_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Connector_Punctuation_Set.Items,
            UCD.General_Category.Pc_Table_2x1,
            UCD.General_Category.Pc_Table_2x2);
      end if;
      return Connector_Punctuation_Set;
   end Connector_Punctuation;

   --  General_Category=Other_Punctuation (Po)

   Other_Punctuation_Set : Character_Set_Access := null;

   function Other_Punctuation return not null access Character_Set is
   begin
      if Other_Punctuation_Set = null then
         Other_Punctuation_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Po_Table_2x1'Length +
               UCD.General_Category.Po_Table_4x1'Length +
               UCD.General_Category.Po_Table_2x2'Length +
               UCD.General_Category.Po_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Other_Punctuation_Set.Items,
            UCD.General_Category.Po_Table_2x1,
            UCD.General_Category.Po_Table_4x1,
            UCD.General_Category.Po_Table_2x2,
            UCD.General_Category.Po_Table_4x2);
      end if;
      return Other_Punctuation_Set;
   end Other_Punctuation;

   --  General_Category=Math_Symbol (Sm)

   Math_Symbol_Set : Character_Set_Access := null;

   function Math_Symbol return not null access Character_Set is
   begin
      if Math_Symbol_Set = null then
         Math_Symbol_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Sm_Table_2x1'Length +
               UCD.General_Category.Sm_Table_4x1'Length +
               UCD.General_Category.Sm_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Math_Symbol_Set.Items,
            UCD.General_Category.Sm_Table_2x1,
            UCD.General_Category.Sm_Table_4x1,
            UCD.General_Category.Sm_Table_2x2);
      end if;
      return Math_Symbol_Set;
   end Math_Symbol;

   --  General_Category=Currency_Symbol (Sc)

   Currency_Symbol_Set : Character_Set_Access := null;

   function Currency_Symbol return not null access Character_Set is
   begin
      if Currency_Symbol_Set = null then
         Currency_Symbol_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Sc_Table_2x1'Length +
               UCD.General_Category.Sc_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Currency_Symbol_Set.Items,
            UCD.General_Category.Sc_Table_2x1,
            UCD.General_Category.Sc_Table_2x2);
      end if;
      return Currency_Symbol_Set;
   end Currency_Symbol;

   --  General_Category=Modifier_Symbol (Sk)

   Modifier_Symbol_Set : Character_Set_Access := null;

   function Modifier_Symbol return not null access Character_Set is
   begin
      if Modifier_Symbol_Set = null then
         Modifier_Symbol_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Sk_Table_2x1'Length +
               UCD.General_Category.Sk_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Modifier_Symbol_Set.Items,
            UCD.General_Category.Sk_Table_2x1,
            UCD.General_Category.Sk_Table_2x2);
      end if;
      return Modifier_Symbol_Set;
   end Modifier_Symbol;

   --  General_Category=Other_Symbol (So)

   Other_Symbol_Set : Character_Set_Access := null;

   function Other_Symbol return not null access Character_Set is
   begin
      if Other_Symbol_Set = null then
         Other_Symbol_Set := new Character_Set'(
            Length =>
               UCD.General_Category.So_Table_2x1'Length +
               UCD.General_Category.So_Table_4x1'Length +
               UCD.General_Category.So_Table_2x2'Length +
               UCD.General_Category.So_Table_4x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Other_Symbol_Set.Items,
            UCD.General_Category.So_Table_2x1,
            UCD.General_Category.So_Table_4x1,
            UCD.General_Category.So_Table_2x2,
            UCD.General_Category.So_Table_4x2);
      end if;
      return Other_Symbol_Set;
   end Other_Symbol;

   --  General_Category=Initial_Punctuation (Pi)

   Initial_Punctuation_Set : Character_Set_Access := null;

   function Initial_Punctuation return not null access Character_Set is
   begin
      if Initial_Punctuation_Set = null then
         Initial_Punctuation_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Pi_Table_2x1'Length +
               UCD.General_Category.Pi_Table_2x2'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Initial_Punctuation_Set.Items,
            UCD.General_Category.Pi_Table_2x1,
            UCD.General_Category.Pi_Table_2x2);
      end if;
      return Initial_Punctuation_Set;
   end Initial_Punctuation;

   --  General_Category=Final_Punctuation (Pf)

   Final_Punctuation_Set : Character_Set_Access := null;

   function Final_Punctuation return not null access Character_Set is
   begin
      if Final_Punctuation_Set = null then
         Final_Punctuation_Set := new Character_Set'(
            Length =>
               UCD.General_Category.Pf_Table_2x1'Length,
            Reference_Count => -1, --  constant
            Items => <>);
         Fill (
            Final_Punctuation_Set.Items,
            UCD.General_Category.Pf_Table_2x1);
      end if;
      return Final_Punctuation_Set;
   end Final_Punctuation;

end Ada.Characters.Inside.Sets.General_Category;
