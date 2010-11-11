pragma License (Unrestricted);
with Ada.Containers;
function Ada.Strings.Hash_Case_Insensitive (Key : String)
   return Containers.Hash_Type;
--  pragma Pure (Hash_Case_Insensitive);
pragma Preelaborate (Hash_Case_Insensitive); --  use maps
