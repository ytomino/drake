pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Canonical_Composites is
   pragma Preelaborate;

   --  not-decomposable characters.
   function Base_Set return not null access Character_Set;

   --  decomposes and extracts the base character.
   function Base_Map return not null access Character_Mapping;

end Ada.Strings.Naked_Maps.Canonical_Composites;
