pragma License (Unrestricted);
with Ada.Containers;
with Ada.Strings.Wide_Wide_Hash;
function Ada.Strings.Wide_Wide_Fixed.Hash (Key : Wide_Wide_String)
   return Containers.Hash_Type
   renames Wide_Wide_Hash;
pragma Pure (Ada.Strings.Wide_Wide_Fixed.Hash);
