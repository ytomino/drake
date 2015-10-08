pragma License (Unrestricted);
--  implementation unit
generic
package Ada.Containers.Ordered_Sets.Debug is
   pragma Preelaborate;

   procedure Dump (Source : Set; Message : String := "");

   function Valid (Source : Set) return Boolean;

end Ada.Containers.Ordered_Sets.Debug;
