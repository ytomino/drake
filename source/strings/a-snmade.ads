pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Debug is
   pragma Preelaborate;

   --  sets

   function Valid (Set : Character_Set_Data) return Boolean;

   --  maps

   function Valid (Map : Character_Mapping_Data) return Boolean;

end Ada.Strings.Naked_Maps.Debug;
