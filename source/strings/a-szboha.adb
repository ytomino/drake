with Ada.Strings.Wide_Wide_Hash;
function Ada.Strings.Wide_Wide_Bounded.Hash (
   Key : Bounded.Bounded_Wide_Wide_String)
   return Containers.Hash_Type is
begin
   return Strings.Wide_Wide_Hash (Key.Element (1 .. Key.Length));
end Ada.Strings.Wide_Wide_Bounded.Hash;
