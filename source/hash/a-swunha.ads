pragma License (Unrestricted);
with Ada.Containers;
with Ada.Strings.Wide_Hash;
with Ada.Strings.Unbounded_Wide_Strings;
function Ada.Strings.Wide_Unbounded.Hash is
   new Unbounded_Wide_Strings.Generic_Hash (Containers.Hash_Type, Wide_Hash);
pragma Preelaborate (Ada.Strings.Wide_Unbounded.Hash);
