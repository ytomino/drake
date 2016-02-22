pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Case_Mapping is
   pragma Preelaborate;

   function Lower_Case_Map return not null Character_Mapping_Access;
   function Upper_Case_Map return not null Character_Mapping_Access;

end Ada.Strings.Naked_Maps.Case_Mapping;
