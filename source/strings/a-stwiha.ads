pragma License (Unrestricted);
with Ada.Strings.Generic_Hash;
function Ada.Strings.Wide_Hash is
   new Generic_Hash (Wide_Character, Wide_String);
pragma Pure (Ada.Strings.Wide_Hash);
