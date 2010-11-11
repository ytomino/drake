pragma License (Unrestricted);
with Ada.Strings.Generic_Unbounded.Hash;
with Ada.Strings.Wide_Hash;
function Ada.Strings.Wide_Unbounded.Hash is new Instance.Hash (Wide_Hash);
pragma Preelaborate (Ada.Strings.Wide_Unbounded.Hash);
