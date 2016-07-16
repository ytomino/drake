pragma Check_Policy (Validate => Disable);
--  with Ada.Strings.Naked_Maps.Debug;
with Ada.UCD.Simple_Case_Mapping;
with System.Once;
with System.Reference_Counting;
package body Ada.Strings.Naked_Maps.Case_Mapping is
   use type UCD.Difference_Base;
   use type UCD.UCS_4;

   procedure Decode (
      Mapping : in out Character_Mapping_Data;
      I : in out Positive;
      Table : UCD.Map_16x1_Type);
   procedure Decode (
      Mapping : in out Character_Mapping_Data;
      I : in out Positive;
      Table : UCD.Map_16x1_Type) is
   begin
      for J in Table'Range loop
         declare
            F : UCD.Map_16x1_Item_Type renames Table (J);
         begin
            Mapping.From (I) := Character_Type'Val (F.Code);
            Mapping.To (I) := Character_Type'Val (F.Mapping);
         end;
         I := I + 1;
      end loop;
   end Decode;

   procedure Decode (
      Mapping : in out Character_Mapping_Data;
      I : in out Positive;
      Table : UCD.Simple_Case_Mapping.Compressed_Type;
      Offset : UCD.Difference_Base);
   procedure Decode (
      Mapping : in out Character_Mapping_Data;
      I : in out Positive;
      Table : UCD.Simple_Case_Mapping.Compressed_Type;
      Offset : UCD.Difference_Base) is
   begin
      for J in Table'Range loop
         declare
            F : UCD.Simple_Case_Mapping.Compressed_Item_Type
               renames Table (J);
            From : Character_Type := Character_Type'Val (
               UCD.Difference_Base (F.Start) + Offset);
            To : Character_Type := Character_Type'Val (
               Character_Type'Pos (From) + F.Diff);
         begin
            for K in 1 .. F.Length loop
               Mapping.From (I) := From;
               Mapping.To (I) := To;
               From := Character_Type'Succ (From);
               To := Character_Type'Succ (To);
               I := I + 1;
            end loop;
         end;
      end loop;
   end Decode;

   procedure Decode_Reverse (
      Mapping : in out Character_Mapping_Data;
      I : in out Positive;
      Table : UCD.Map_16x1_Type);
   procedure Decode_Reverse (
      Mapping : in out Character_Mapping_Data;
      I : in out Positive;
      Table : UCD.Map_16x1_Type) is
   begin
      for J in Table'Range loop
         declare
            F : UCD.Map_16x1_Item_Type renames Table (J);
         begin
            Mapping.From (I) := Character_Type'Val (F.Mapping);
            Mapping.To (I) := Character_Type'Val (F.Code);
         end;
         I := I + 1;
      end loop;
   end Decode_Reverse;

   procedure Decode_Reverse (
      Mapping : in out Character_Mapping_Data;
      I : in out Positive;
      Table : UCD.Simple_Case_Mapping.Compressed_Type;
      Offset : UCD.Difference_Base);
   procedure Decode_Reverse (
      Mapping : in out Character_Mapping_Data;
      I : in out Positive;
      Table : UCD.Simple_Case_Mapping.Compressed_Type;
      Offset : UCD.Difference_Base) is
   begin
      for J in Table'Range loop
         declare
            F : UCD.Simple_Case_Mapping.Compressed_Item_Type renames Table (J);
            To : Character_Type := Character_Type'Val (
               UCD.Difference_Base (F.Start) + Offset);
            From : Character_Type := Character_Type'Val (
               Character_Type'Pos (To) + F.Diff);
         begin
            for K in 1 .. F.Length loop
               Mapping.From (I) := From;
               Mapping.To (I) := To;
               From := Character_Type'Succ (From);
               To := Character_Type'Succ (To);
               I := I + 1;
            end loop;
         end;
      end loop;
   end Decode_Reverse;

   type Character_Mapping_Access_With_Pool is access Character_Mapping_Data;

   --  lower case map

   L_Mapping : Character_Mapping_Access_With_Pool;
   L_Mapping_Flag : aliased System.Once.Flag := 0;

   procedure L_Mapping_Init;
   procedure L_Mapping_Init is
      Mapping : Character_Mapping_Access_With_Pool
         renames L_Mapping;
   begin
      Mapping := new Character_Mapping_Data'(
         Length => UCD.Simple_Case_Mapping.L_Total,
         Reference_Count => System.Reference_Counting.Static,
         From => <>,
         To => <>);
      declare
         I : Positive := Mapping.From'First;
      begin
         --  16#0041#
         Decode (
            Mapping.all,
            I,
            UCD.Simple_Case_Mapping.SL_Table_XXXX_Compressed,
            Offset => 0);
         --  16#0100# ..
         Decode (
            Mapping.all,
            I,
            UCD.Simple_Case_Mapping.SL_Table_XXXX);
         --  16#0130# ..
         Decode (
            Mapping.all,
            I,
            UCD.Simple_Case_Mapping.DL_Table_XXXX);
         --  16#10400#
         Decode (
            Mapping.all,
            I,
            UCD.Simple_Case_Mapping.SL_Table_1XXXX_Compressed,
            Offset => 16#10000#);
         pragma Assert (I = Mapping.From'Last + 1);
      end;
      Sort (Mapping.From, Mapping.To);
      pragma Check (Validate, Debug.Valid (Mapping.all));
   end L_Mapping_Init;

   --  implementation of lower case map

   function Lower_Case_Map return not null Character_Mapping_Access is
   begin
      System.Once.Initialize (L_Mapping_Flag'Access, L_Mapping_Init'Access);
      return Character_Mapping_Access (L_Mapping);
   end Lower_Case_Map;

   --  upper case map

   U_Mapping : Character_Mapping_Access_With_Pool;
   U_Mapping_Flag : aliased System.Once.Flag := 0;

   procedure U_Mapping_Init;
   procedure U_Mapping_Init is
      Mapping : Character_Mapping_Access_With_Pool
         renames U_Mapping;
   begin
      Mapping := new Character_Mapping_Data'(
         Length => UCD.Simple_Case_Mapping.U_Total,
         Reference_Count => System.Reference_Counting.Static,
         From => <>,
         To => <>);
      declare
         I : Positive := Mapping.From'First;
      begin
         --  16#0061#
         Decode_Reverse (
            Mapping.all,
            I,
            UCD.Simple_Case_Mapping.SL_Table_XXXX_Compressed,
            Offset => 0);
         --  16#00B5# ..
         Decode (
            Mapping.all,
            I,
            UCD.Simple_Case_Mapping.DU_Table_XXXX);
         --  16#0101# ..
         Decode_Reverse (
            Mapping.all,
            I,
            UCD.Simple_Case_Mapping.SL_Table_XXXX);
         --  16#10440#
         Decode_Reverse (
            Mapping.all,
            I,
            UCD.Simple_Case_Mapping.SL_Table_1XXXX_Compressed,
            Offset => 16#10000#);
         pragma Assert (I = Mapping.From'Last + 1);
      end;
      Sort (Mapping.From, Mapping.To);
      pragma Check (Validate, Debug.Valid (Mapping.all));
   end U_Mapping_Init;

   --  implementation of upper case map

   function Upper_Case_Map return not null Character_Mapping_Access is
   begin
      System.Once.Initialize (U_Mapping_Flag'Access, U_Mapping_Init'Access);
      return Character_Mapping_Access (U_Mapping);
   end Upper_Case_Map;

end Ada.Strings.Naked_Maps.Case_Mapping;
