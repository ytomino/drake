pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Hash
with Ada.Containers;
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
function Ada.Strings.Generic_Hash (Key : String_Type)
   return Containers.Hash_Type;
pragma Pure (Ada.Strings.Generic_Hash);
