with Ada.UCD.Simple_Case_Mapping;
package body Ada.Characters.Inside.Maps.Upper_Case is
   pragma Suppress (All_Checks);

   Mapping : access Character_Mapping;

   function Upper_Case_Map return not null access Character_Mapping is
   begin
      if Mapping = null then
         Mapping := new Character_Mapping'(
            Length =>
               UCD.Simple_Case_Mapping.Shared_Lower_Table_2'Length +
               UCD.Simple_Case_Mapping.Shared_Lower_Table_4'Length +
               UCD.Simple_Case_Mapping.Difference_Upper_Table_2'Length,
            Reference_Count => -1, -- constant
            From => <>,
            To => <>);
         declare
            I : Positive := Mapping.From'First;
         begin
            for J in UCD.Simple_Case_Mapping.Shared_Lower_Table_2'Range loop
               Mapping.From (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Shared_Lower_Table_2 (J).Mapping);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Shared_Lower_Table_2 (J).Code);
               I := I + 1;
            end loop;
            for J in UCD.Simple_Case_Mapping.Shared_Lower_Table_4'Range loop
               Mapping.From (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Shared_Lower_Table_4 (J).Mapping);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Shared_Lower_Table_4 (J).Code);
               I := I + 1;
            end loop;
            for J in
               UCD.Simple_Case_Mapping.Difference_Upper_Table_2'Range
            loop
               Mapping.From (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Difference_Upper_Table_2 (J).Code);
               Mapping.To (I) := Character_Type'Val (
                  UCD.Simple_Case_Mapping.Difference_Upper_Table_2 (
                     J).Mapping);
               I := I + 1;
            end loop;
            pragma Assert (I = Mapping.From'Last + 1);
         end;
         Sort (Mapping);
      end if;
      return Mapping;
   end Upper_Case_Map;

end Ada.Characters.Inside.Maps.Upper_Case;
