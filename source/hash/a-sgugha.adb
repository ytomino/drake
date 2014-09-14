function Ada.Strings.Generic_Unbounded.Generic_Hash (Key : Unbounded_String)
   return Ada.Containers.Hash_Type is
begin
   return Fixed_Hash (Key.Data.Items (1 .. Key.Length));
end Ada.Strings.Generic_Unbounded.Generic_Hash;
