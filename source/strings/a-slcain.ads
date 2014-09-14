pragma License (Unrestricted);
--  Ada 2012
with Ada.Characters.Conversions;
with Ada.Strings.Generic_Less_Case_Insensitive;
function Ada.Strings.Less_Case_Insensitive is
   new Generic_Less_Case_Insensitive (
      Character,
      String,
      Characters.Conversions.Get);
--  pragma Pure (Ada.Strings.Less_Case_Insensitive);
pragma Preelaborate (Ada.Strings.Less_Case_Insensitive); -- use maps
