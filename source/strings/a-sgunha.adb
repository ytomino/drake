function Ada.Strings.Generic_Unbounded.Hash (Key : Unbounded_String)
   return Containers.Hash_Type is
begin
   return Fixed_Hash (Constant_Reference (Key'Access).Element.all);
end Ada.Strings.Generic_Unbounded.Hash;
