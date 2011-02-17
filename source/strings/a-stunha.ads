pragma License (Unrestricted);
with Ada.Strings.Generic_Unbounded.Hash;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded_Strings;
function Ada.Strings.Unbounded.Hash is
   new Unbounded_Strings.Hash (Strings.Hash);
pragma Preelaborate (Ada.Strings.Unbounded.Hash);
