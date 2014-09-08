pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Hash
with Ada.Containers;
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   with procedure Get (
      Item : String_Type;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean);
function Ada.Strings.Generic_Hash (Key : String_Type)
   return Containers.Hash_Type;
pragma Pure (Ada.Strings.Generic_Hash);
