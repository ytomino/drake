with Ada.UCD.Simple_Case_Mapping;
with System.Once;
with System.Reference_Counting;
package body Ada.Characters.Inside.Maps.Upper_Case is
   use type UCD.Difference_Base;
   use type UCD.UCS_4;

   Mapping : access Character_Mapping;
   Mapping_Flag : aliased System.Once.Flag := 0;

   procedure Decode (
      Mapping : not null access Character_Mapping;
      I : in out Positive;
      Table : UCD.Map_16x1_Type);
   procedure Decode (
      Mapping : not null access Character_Mapping;
      I : in out Positive;
      Table : UCD.Map_16x1_Type) is
   begin
      for J in Table'Range loop
         declare
            Item : UCD.Map_16x1_Item_Type renames Table (J);
         begin
            Mapping.From (I) := Character_Type'Val (Item.Code);
            Mapping.To (I) := Character_Type'Val (Item.Mapping);
         end;
         I := I + 1;
      end loop;
   end Decode;

   procedure Decode_Reverse (
      Mapping : not null access Character_Mapping;
      I : in out Positive;
      Table : UCD.Map_16x1_Type);
   procedure Decode_Reverse (
      Mapping : not null access Character_Mapping;
      I : in out Positive;
      Table : UCD.Map_16x1_Type) is
   begin
      for J in Table'Range loop
         declare
            Item : UCD.Map_16x1_Item_Type renames Table (J);
         begin
            Mapping.From (I) := Character_Type'Val (Item.Mapping);
            Mapping.To (I) := Character_Type'Val (Item.Code);
         end;
         I := I + 1;
      end loop;
   end Decode_Reverse;

   procedure Decode_Reverse (
      Mapping : not null access Character_Mapping;
      I : in out Positive;
      Table : UCD.Simple_Case_Mapping.Compressed_Type;
      Offset : UCD.Difference_Base);
   procedure Decode_Reverse (
      Mapping : not null access Character_Mapping;
      I : in out Positive;
      Table : UCD.Simple_Case_Mapping.Compressed_Type;
      Offset : UCD.Difference_Base) is
   begin
      for J in Table'Range loop
         declare
            Item : UCD.Simple_Case_Mapping.Compressed_Item_Type
               renames Table (J);
            To : Character_Type := Character_Type'Val (
               UCD.Difference_Base (Item.Start) + Offset);
            From : Character_Type := Character_Type'Val (
               Character_Type'Pos (To) + Item.Diff);
         begin
            for K in 1 .. Item.Length loop
               Mapping.From (I) := From;
               Mapping.To (I) := To;
               From := Character_Type'Succ (From);
               To := Character_Type'Succ (To);
               I := I + 1;
            end loop;
         end;
      end loop;
   end Decode_Reverse;

   procedure Mapping_Init;
   procedure Mapping_Init is
   begin
      Mapping := new Character_Mapping'(
         Length => UCD.Simple_Case_Mapping.U_Total,
         Reference_Count => System.Reference_Counting.Static,
         From => <>,
         To => <>);
      declare
         I : Positive := Mapping.From'First;
      begin
         --  16#0061#
         Decode_Reverse (Mapping, I,
            UCD.Simple_Case_Mapping.SL_Table_XXXX_Compressed, Offset => 0);
         --  16#00B5# ..
         Decode (Mapping, I, UCD.Simple_Case_Mapping.DU_Table_XXXX);
         --  16#0101# ..
         Decode_Reverse (Mapping, I, UCD.Simple_Case_Mapping.SL_Table_XXXX);
         --  16#10440#
         Decode_Reverse (Mapping, I,
            UCD.Simple_Case_Mapping.SL_Table_1XXXX_Compressed,
            Offset => 16#10000#);
         pragma Assert (I = Mapping.From'Last + 1);
      end;
      Sort (Mapping.From, Mapping.To);
   end Mapping_Init;

   --  implementation

   function Upper_Case_Map return not null access Character_Mapping is
   begin
      System.Once.Initialize (Mapping_Flag'Access, Mapping_Init'Access);
      return Mapping;
   end Upper_Case_Map;

end Ada.Characters.Inside.Maps.Upper_Case;
