pragma License (Unrestricted);
--  implementation unit
package Ada.Characters.Inside.Maps.Case_Mapping is
   pragma Preelaborate;

   function Lower_Case_Map return not null access Character_Mapping;
   function Upper_Case_Map return not null access Character_Mapping;

end Ada.Characters.Inside.Maps.Case_Mapping;
