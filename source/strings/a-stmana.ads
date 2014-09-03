pragma License (Unrestricted);
--  implementation unit
with Ada.Strings.Naked_Maps;
package Ada.Strings.Maps.Naked is
   pragma Preelaborate;

   generic
      with function Source
         return not null access Naked_Maps.Character_Set;
   function To_Set return Character_Set;
   pragma Inline (To_Set);

   generic
      with function Source
         return not null access Naked_Maps.Character_Mapping;
   function To_Mapping return Character_Mapping;
   pragma Inline (To_Mapping);

end Ada.Strings.Maps.Naked;
