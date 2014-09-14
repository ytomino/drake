pragma License (Unrestricted);
--  Ada 2012
with Ada.Characters.Conversions;
with Ada.Strings.Generic_Equal_Case_Insensitive;
function Ada.Strings.Equal_Case_Insensitive is
   new Generic_Equal_Case_Insensitive (
      Character,
      String,
      Characters.Conversions.Get);
--  pragma Pure (Ada.Strings.Equal_Case_Insensitive);
pragma Preelaborate (Ada.Strings.Equal_Case_Insensitive); -- use maps
