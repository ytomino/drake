function Ada.Strings.Generic_Unbounded.Generic_Hash (Key : Unbounded_String)
   return Containers.Hash_Type
is
   pragma Suppress (Access_Check);
begin
   return Fixed_Hash (Key.Data.Items (1 .. Key.Length));
end Ada.Strings.Generic_Unbounded.Generic_Hash;
