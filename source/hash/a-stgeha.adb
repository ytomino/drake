with Ada.Containers.Murmur_Hash_3;
function Ada.Strings.Generic_Hash (Key : String_Type)
   return Containers.Hash_Type
is
   use type Containers.Hash_Type;
   State : Containers.Murmur_Hash_3.State :=
      Containers.Murmur_Hash_3.Initialize (0);
   Result : Containers.Hash_Type;
begin
   for I in Key'Range loop
      Containers.Murmur_Hash_3.Update (
         State,
         Containers.Hash_Type'(Character_Type'Pos (Key (I))));
   end loop;
   Containers.Murmur_Hash_3.Update (State, Containers.Count_Type'(Key'Length));
   Containers.Murmur_Hash_3.Finalize (State, Result);
   return Result;
end Ada.Strings.Generic_Hash;
