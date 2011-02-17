pragma License (Unrestricted);
with Ada.Strings.Generic_Unbounded.Hash;
with Ada.Strings.Wide_Hash;
with Ada.Strings.Unbounded_Wide_Strings;
function Ada.Strings.Wide_Unbounded.Hash is
   new Unbounded_Wide_Strings.Hash (Wide_Hash);
pragma Preelaborate (Ada.Strings.Wide_Unbounded.Hash);
