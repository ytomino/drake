pragma License (Unrestricted);
with Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded_Strings;
function Ada.Strings.Unbounded.Hash is
   new Unbounded_Strings.Generic_Hash (Containers.Hash_Type, Strings.Hash);
pragma Preelaborate (Ada.Strings.Unbounded.Hash);
