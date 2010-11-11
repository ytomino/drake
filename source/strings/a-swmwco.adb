pragma License (Unrestricted);
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
package body Ada.Strings.Wide_Maps.Wide_Constants is

   function Lower_Case_Map return Wide_Character_Mapping is
   begin
      return Wide_Character_Mapping (
         Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map);
   end Lower_Case_Map;

end Ada.Strings.Wide_Maps.Wide_Constants;
