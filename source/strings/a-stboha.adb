with Ada.Strings.Hash;
function Ada.Strings.Bounded.Hash (
   Key : Bounded.Bounded_String)
   return Containers.Hash_Type is
begin
   return Strings.Hash (Key.Data (1 .. Key.Length));
end Ada.Strings.Bounded.Hash;
