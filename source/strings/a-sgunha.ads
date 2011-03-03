pragma License (Unrestricted);
with Ada.Containers;
generic
   with function Fixed_Hash (Key : String_Type) return Containers.Hash_Type;
function Ada.Strings.Generic_Unbounded.Hash (Key : Unbounded_String)
   return Containers.Hash_Type;
pragma Preelaborate (Ada.Strings.Generic_Unbounded.Hash);
