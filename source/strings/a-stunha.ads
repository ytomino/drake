pragma License (Unrestricted);
with Ada.Strings.Generic_Unbounded.Hash;
with Ada.Strings.Hash;
function Ada.Strings.Unbounded.Hash is new Instance.Hash (Strings.Hash);
pragma Preelaborate (Ada.Strings.Unbounded.Hash);
