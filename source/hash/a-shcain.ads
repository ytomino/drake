pragma License (Unrestricted);
--  Ada 2012
with Ada.Characters.Conversions;
with Ada.Strings.Generic_Hash_Case_Insensitive;
function Ada.Strings.Hash_Case_Insensitive is
   new Generic_Hash_Case_Insensitive (
      Character,
      String,
      Characters.Conversions.Get);
--  pragma Pure (Ada.Strings.Hash_Case_Insensitive);
pragma Preelaborate (Ada.Strings.Hash_Case_Insensitive); -- use maps
