pragma License (Unrestricted);
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
package body Ada.Strings.Maps.Constants is

   function Lower_Case_Map return Character_Mapping is
   begin
      return Character_Mapping (
         Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map);
   end Lower_Case_Map;

end Ada.Strings.Maps.Constants;
