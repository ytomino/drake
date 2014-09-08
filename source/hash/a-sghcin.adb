with Ada.Strings.Naked_Maps.Case_Folding;
function Ada.Strings.Generic_Hash_Case_Insensitive (Key : String_Type)
   return Containers.Hash_Type
is
   use type Containers.Hash_Type;
   Result : Containers.Hash_Type := 0;
   I : Natural := Key'First;
begin
   while I <= Key'Last loop
      declare
         Code : Wide_Wide_Character;
         Next : Natural;
         Is_Illegal_Sequence : Boolean; -- ignore
      begin
         --  get single unicode character
         Get (
            Key (I .. Key'Last),
            Next,
            Code,
            Is_Illegal_Sequence);
         I := Next + 1;
         --  update
         Result := Containers.Rotate_Left (Result, 5)
            xor Wide_Wide_Character'Pos (
               Strings.Naked_Maps.Value (
                  Strings.Naked_Maps.Case_Folding.Case_Folding_Map.all,
                  Code));
      end;
   end loop;
   return Result;
end Ada.Strings.Generic_Hash_Case_Insensitive;
