pragma License (Unrestricted);
with Ada.Containers;
generic
   with package Bounded is new Generic_Bounded_Length (<>);
function Ada.Strings.Wide_Bounded.Hash (
   Key : Bounded.Bounded_Wide_String)
   return Containers.Hash_Type;
pragma Preelaborate (Ada.Strings.Wide_Bounded.Hash);
