with Ada.UCD.Simple_Case_Mapping;
with System.Once;
with System.Reference_Counting;
package body Ada.Characters.Inside.Maps.Lower_Case is
   use type UCD.UCS_4;

   Mapping : access Character_Mapping;
   Mapping_Flag : aliased System.Once.Flag := 0;

   procedure Mapping_Init;
   procedure Mapping_Init is
   begin
      Mapping := new Character_Mapping'(
         Length =>
            UCD.Simple_Case_Mapping.Shared_Lower_Table_2'Length
            + UCD.Simple_Case_Mapping.Shared_Lower_Table_4'Length
            + UCD.Simple_Case_Mapping.Difference_Lower_Table_2'Length,
         Reference_Count => System.Reference_Counting.Static,
         From => <>,
         To => <>);
      declare
         I : Positive := Mapping.From'First;
         J : Positive := UCD.Simple_Case_Mapping.Shared_Lower_Table_2'First;
         K : Positive :=
            UCD.Simple_Case_Mapping.Difference_Lower_Table_2'First;
      begin
         while K <=
            UCD.Simple_Case_Mapping.Difference_Lower_Table_2'Last
         loop
            if UCD.Simple_Case_Mapping.Shared_Lower_Table_2 (J).Code <
               UCD.Simple_Case_Mapping.Difference_Lower_Table_2 (K).Code
            then
               Mapping.From (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Shared_Lower_Table_2 (J).Code);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Shared_Lower_Table_2 (J).Mapping);
               J := J + 1;
            else
               Mapping.From (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Difference_Lower_Table_2 (
                     K).Code);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Difference_Lower_Table_2 (
                     K).Mapping);
               K := K + 1;
            end if;
            I := I + 1;
         end loop;
         while J <= UCD.Simple_Case_Mapping.Shared_Lower_Table_2'Last loop
            Mapping.From (I) := Character_Type'Val (
               UCD.Simple_Case_Mapping.Shared_Lower_Table_2 (J).Code);
            Mapping.To (I) := Character_Type'Val (
               UCD.Simple_Case_Mapping.Shared_Lower_Table_2 (J).Mapping);
            J := J + 1;
            I := I + 1;
         end loop;
         for L in UCD.Simple_Case_Mapping.Shared_Lower_Table_4'Range loop
            Mapping.From (I) := Character_Type'Val (
               UCD.Simple_Case_Mapping.Shared_Lower_Table_4 (L).Code);
            Mapping.To (I) := Character_Type'Val (
               UCD.Simple_Case_Mapping.Shared_Lower_Table_4 (L).Mapping);
            I := I + 1;
         end loop;
         pragma Assert (I = Mapping.From'Last + 1);
      end;
   end Mapping_Init;

   --  implementation

   function Lower_Case_Map return not null access Character_Mapping is
   begin
      System.Once.Initialize (Mapping_Flag'Access, Mapping_Init'Access);
      return Mapping;
   end Lower_Case_Map;

end Ada.Characters.Inside.Maps.Lower_Case;
