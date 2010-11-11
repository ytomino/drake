function Ada.Strings.Generic_Unbounded.Hash (Key : Unbounded_String)
   return Containers.Hash_Type is
begin
   if Key.Length = 0 then
      pragma Assert (Containers."=" (Fixed_Hash ((1 .. 0 => <>)), 0));
      return 0;
   else
      return Fixed_Hash (Key.Data.Items (1 .. Key.Length));
   end if;
end Ada.Strings.Generic_Unbounded.Hash;
