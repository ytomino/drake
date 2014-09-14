pragma License (Unrestricted);
with Ada.Characters.Conversions;
with Ada.Strings.Generic_Hash;
function Ada.Strings.Wide_Hash is
   new Generic_Hash (
      Wide_Character,
      Wide_String,
      Characters.Conversions.Get);
pragma Pure (Ada.Strings.Wide_Hash);
