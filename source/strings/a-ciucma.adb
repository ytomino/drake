with Ada.UCD.Case_Folding;
package body Ada.Characters.Inside.Upper_Case_Maps is
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

   function Upper_Case_Map return not null access Character_Mapping is
   begin
      if not Initialized then
         declare
            I : Positive := Mapping.From'First;
         begin
            for J in UCD.Case_Folding.S_Table'Range loop
               Mapping.From (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_4 (J).Mapping);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_4 (J).Code);
               I := I + 1;
            end loop;
            for J in UCD.Case_Folding.C_Table_2'Range loop
               Mapping.From (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_4 (J).Mapping);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_4 (J).Code);
               I := I + 1;
            end loop;
            for J in UCD.Case_Folding.C_Table_4'Range loop
               Mapping.From (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_4 (J).Mapping);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Case_Folding.C_Table_4 (J).Code);
               I := I + 1;
            end loop;
            pragma Assert (I = Mapping.From'Last + 1);
         end;
         Sort (Mapping'Access);
         Initialized := True;
      end if;
      return Mapping'Access;
   end Upper_Case_Map;

end Ada.Characters.Inside.Upper_Case_Maps;
