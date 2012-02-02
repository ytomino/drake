with Ada.UCD.Case_Folding;
with System.Once;
with System.Reference_Counting;
package body Ada.Characters.Inside.Maps.Case_Folding is
   pragma Suppress (All_Checks);
   use type UCD.UCS_4;

   Mapping : access Character_Mapping;
   Mapping_Flag : aliased System.Once.Flag := 0;

   procedure Mapping_Init;
   procedure Mapping_Init is
   begin
      Mapping := new Character_Mapping'(
         Length =>
            UCD.Case_Folding.C_Table_2'Length
            + UCD.Case_Folding.C_Table_4'Length
            + UCD.Case_Folding.S_Table'Length,
         Reference_Count => System.Reference_Counting.Static,
         From => <>,
         To => <>);
      declare
         I : Positive := Mapping.From'First;
         J : Positive := UCD.Case_Folding.C_Table_2'First;
         K : Positive := UCD.Case_Folding.S_Table'First;
      begin
         while K <= UCD.Case_Folding.S_Table'Last loop
            if UCD.Case_Folding.C_Table_2 (J).Code <
               UCD.Case_Folding.S_Table (K).Code
            then
               Mapping.From (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_2 (J).Code);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_2 (J).Mapping);
               J := J + 1;
            else
               Mapping.From (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_2 (K).Code);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_2 (K).Mapping);
               K := K + 1;
            end if;
            I := I + 1;
         end loop;
         while J <= UCD.Case_Folding.C_Table_2'Last loop
            Mapping.From (I) := Character_Type'Val (
               UCD.Case_Folding.C_Table_2 (J).Code);
            Mapping.To (I) := Character_Type'Val (
               UCD.Case_Folding.C_Table_2 (J).Mapping);
            J := J + 1;
            I := I + 1;
         end loop;
         for L in UCD.Case_Folding.C_Table_4'Range loop
            Mapping.From (I) := Character_Type'Val (
               UCD.Case_Folding.C_Table_4 (L).Code);
            Mapping.To (I) := Character_Type'Val (
               UCD.Case_Folding.C_Table_4 (L).Mapping);
            I := I + 1;
         end loop;
         pragma Assert (I = Mapping.From'Last + 1);
      end;
   end Mapping_Init;

   --  implementation

   function Case_Folding_Map return not null access Character_Mapping is
   begin
      System.Once.Initialize (Mapping_Flag'Access, Mapping_Init'Access);
      return Mapping;
   end Case_Folding_Map;

end Ada.Characters.Inside.Maps.Case_Folding;
