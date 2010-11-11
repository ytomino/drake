pragma License (Unrestricted);
with Ada.Strings.Generic_Bounded;
--  with Ada.Strings.Maps;
package Ada.Strings.Bounded is
   pragma Preelaborate;

   --  if this package instantiate Generic_Bounded directly,
   --  Bounded.Hash can not have body...
   package Instance is new Generic_Bounded (Character, String);

   generic package Generic_Bounded_Length
      renames Instance.Generic_Bounded_Length;

end Ada.Strings.Bounded;
