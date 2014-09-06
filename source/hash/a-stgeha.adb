function Ada.Strings.Generic_Hash (Key : String_Type)
   return Containers.Hash_Type
is
   use type Containers.Hash_Type;
   Result : Containers.Hash_Type := 0;
begin
   for I in Key'Range loop
      Result := Containers.Rotate_Left (Result, 5) xor
         Character_Type'Pos (Key (I));
   end loop;
   return Result;
end Ada.Strings.Generic_Hash;
