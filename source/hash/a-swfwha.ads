pragma License (Unrestricted);
with Ada.Containers;
with Ada.Strings.Wide_Hash;
function Ada.Strings.Wide_Fixed.Wide_Hash (Key : Wide_String)
   return Containers.Hash_Type
   renames Strings.Wide_Hash;
pragma Pure (Ada.Strings.Wide_Fixed.Wide_Hash);
