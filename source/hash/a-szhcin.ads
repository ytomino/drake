pragma License (Unrestricted);
--  Ada 2012
with Ada.Characters.Conversions;
with Ada.Strings.Generic_Hash_Case_Insensitive;
function Ada.Strings.Wide_Wide_Hash_Case_Insensitive is
   new Generic_Hash_Case_Insensitive (
      Wide_Wide_Character,
      Wide_Wide_String,
      Characters.Conversions.Get);
--  pragma Pure (Ada.Strings.Wide_Wide_Hash_Case_Insensitive);
pragma Preelaborate (Ada.Strings.Wide_Wide_Hash_Case_Insensitive); -- use maps
