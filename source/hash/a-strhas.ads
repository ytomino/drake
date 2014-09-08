pragma License (Unrestricted);
with Ada.Characters.Conversions;
with Ada.Strings.Generic_Hash;
function Ada.Strings.Hash is
   new Generic_Hash (
      Character,
      String,
      Characters.Conversions.Get);
pragma Pure (Ada.Strings.Hash);
