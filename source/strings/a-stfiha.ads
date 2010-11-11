pragma License (Unrestricted);
with Ada.Containers;
with Ada.Strings.Hash;
function Ada.Strings.Fixed.Hash (Key : String) return Containers.Hash_Type
   renames Ada.Strings.Hash;
pragma Pure (Ada.Strings.Fixed.Hash);
