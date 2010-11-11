pragma License (Unrestricted);
with Ada.Strings.Generic_Hash;
function Ada.Strings.Hash is new Generic_Hash (Character, String);
pragma Pure (Ada.Strings.Hash);
