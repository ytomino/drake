pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Canonical_Composites is
   pragma Preelaborate;

   --  not-decomposable characters.
   function Base_Set return not null Character_Set_Access;

   --  decomposes and extracts the base character.
   function Basic_Map return not null Character_Mapping_Access;

end Ada.Strings.Naked_Maps.Canonical_Composites;
