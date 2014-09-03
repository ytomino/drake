pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Case_Mapping is
   pragma Preelaborate;

   function Lower_Case_Map return not null access Character_Mapping;
   function Upper_Case_Map return not null access Character_Mapping;

end Ada.Strings.Naked_Maps.Case_Mapping;
