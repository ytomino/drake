pragma License (Unrestricted);
--  implementation unit
with Ada.Characters.Inside.Sets;
with Ada.Characters.Inside.Maps;
package Ada.Strings.Maps.Inside is
   pragma Preelaborate;

   generic
      with function Source
         return not null access Characters.Inside.Sets.Character_Set;
   function To_Set return Character_Set;
   pragma Inline (To_Set);

   generic
      with function Source
         return not null access Characters.Inside.Maps.Character_Mapping;
   function To_Mapping return Character_Mapping;
   pragma Inline (To_Mapping);

end Ada.Strings.Maps.Inside;
