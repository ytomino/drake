with Ada.Strings.Wide_Hash;
function Ada.Strings.Wide_Bounded.Hash (
   Key : Bounded.Bounded_Wide_String)
   return Containers.Hash_Type
is
   pragma Suppress (All_Checks);
begin
   return Strings.Wide_Hash (Key.Element (1 .. Key.Length));
end Ada.Strings.Wide_Bounded.Hash;
