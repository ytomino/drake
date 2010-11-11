with Ada.UCD.Case_Folding;
package body Ada.Characters.Inside.Lower_Case_Maps is
   use type UCD.UCS_4;

   Mapping : aliased Character_Mapping := (
      Length =>
         UCD.Case_Folding.C_Table_2'Length +
         UCD.Case_Folding.C_Table_4'Length +
         UCD.Case_Folding.S_Table'Length,
      Reference_Count => -1, --  constant
      From => <>,
      To => <>);

   Initialized : Boolean := False;

   function Lower_Case_Map return not null access Character_Mapping is
   begin
      if not Initialized then
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
         Initialized := True;
      end if;
      return Mapping'Access;
   end Lower_Case_Map;

end Ada.Characters.Inside.Lower_Case_Maps;
