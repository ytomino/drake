pragma License (Unrestricted);
with Ada.Strings.Generic_Unbounded.Generic_Hash;
with Ada.Strings.Wide_Wide_Hash;
function Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash is
   new Unbounded_Wide_Wide_Strings.Generic_Hash (Wide_Wide_Hash);
pragma Preelaborate (Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash);
