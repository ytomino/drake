with Ada.Characters.Inside.Lower_Case_Maps;
package body Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants is

   function Lower_Case_Map return Wide_Wide_Character_Mapping is
   begin
      return (Finalization.Controlled with
         Data => Map_Data_Access (
            Characters.Inside.Lower_Case_Maps.Lower_Case_Map));
   end Lower_Case_Map;

end Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
