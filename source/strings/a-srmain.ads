pragma License (Unrestricted);
--  implementation package
with Ada.Characters.Inside.Sets;
with Ada.Characters.Inside.Maps;
package Ada.Strings.Root_Maps.Inside is
   pragma Preelaborate;

   generic
      type T is new Root_Character_Set;
      with function Source
         return not null access Characters.Inside.Sets.Character_Set;
   function To_Set return T;
   --  pragma Inline_Always (To_Set); -- not affected

   generic
      type T is new Root_Character_Mapping;
      with function Source
         return not null access Characters.Inside.Maps.Character_Mapping;
   function To_Mapping return T;
   --  pragma Inline_Always (To_Mapping); -- not affected

end Ada.Strings.Root_Maps.Inside;
