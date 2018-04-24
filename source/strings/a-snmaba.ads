pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Basic is
   pragma Preelaborate;

   --  not-decomposable letters.
   function Basic_Set return not null Character_Set_Access;

end Ada.Strings.Naked_Maps.Basic;
