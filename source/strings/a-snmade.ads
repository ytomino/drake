pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Debug is
   pragma Preelaborate;

   --  sets

   function Valid (Set : Character_Set) return Boolean;

   --  maps

   function Valid (Map : Character_Mapping) return Boolean;

end Ada.Strings.Naked_Maps.Debug;
