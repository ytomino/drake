pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Basic is
   pragma Preelaborate;

   --  not-decomposable letters.
   function Basic_Set return not null Character_Set_Access;

   --  decomposes and extracts the base character if the character is a letter,
   --  otherwise, it returns the same character.
   function Basic_Map return not null Character_Mapping_Access;

end Ada.Strings.Naked_Maps.Basic;
