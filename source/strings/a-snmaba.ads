pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Basic is
   pragma Preelaborate;

   --  not-decomposable letters.
   function Basic_Set return not null access Character_Set;

   --  decomposes and extracts the base character if the character is a letter,
   --  otherwise, it returns the same character.
   function Basic_Map return not null access Character_Mapping;

end Ada.Strings.Naked_Maps.Basic;
