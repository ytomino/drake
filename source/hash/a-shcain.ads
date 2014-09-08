pragma License (Unrestricted);
--  Ada 2012
with Ada.Containers;
function Ada.Strings.Hash_Case_Insensitive (Key : String)
   return Containers.Hash_Type;
--  pragma Pure (Ada.Strings.Hash_Case_Insensitive);
pragma Preelaborate (Ada.Strings.Hash_Case_Insensitive); -- use maps
