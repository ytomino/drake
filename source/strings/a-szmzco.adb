with Ada.Characters.Inside.Maps.Lower_Case;
with Ada.Characters.Inside.Maps.Upper_Case;
package body Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants is

   function Lower_Case_Map return Wide_Wide_Character_Mapping is
   begin
      return (Finalization.Controlled with
         Data => Map_Data_Access (
            Characters.Inside.Maps.Lower_Case.Lower_Case_Map));
   end Lower_Case_Map;

   function Upper_Case_Map return Wide_Wide_Character_Mapping is
   begin
      return (Finalization.Controlled with
         Data => Map_Data_Access (
            Characters.Inside.Maps.Upper_Case.Upper_Case_Map));
   end Upper_Case_Map;

end Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
