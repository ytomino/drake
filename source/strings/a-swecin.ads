pragma License (Unrestricted);
--  Ada 2012
with Ada.Characters.Conversions;
with Ada.Strings.Generic_Equal_Case_Insensitive;
function Ada.Strings.Wide_Equal_Case_Insensitive is
   new Generic_Equal_Case_Insensitive (
      Wide_Character,
      Wide_String,
      Characters.Conversions.Get);
--  pragma Pure (Ada.Strings.Wide_Equal_Case_Insensitive);
pragma Preelaborate (Ada.Strings.Wide_Equal_Case_Insensitive); -- use maps
