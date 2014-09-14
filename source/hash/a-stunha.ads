pragma License (Unrestricted);
with Ada.Strings.Generic_Unbounded.Generic_Hash;
with Ada.Strings.Hash;
function Ada.Strings.Unbounded.Hash is
   new Unbounded_Strings.Generic_Hash (Strings.Hash);
pragma Preelaborate (Ada.Strings.Unbounded.Hash);
