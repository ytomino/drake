with Ada.Containers.Murmur_Hash_3;
with Ada.Strings.Naked_Maps.Case_Folding;
function Ada.Strings.Generic_Hash_Case_Insensitive (Key : String_Type)
   return Containers.Hash_Type
is
   Mapping : constant not null access Naked_Maps.Character_Mapping :=
      Strings.Naked_Maps.Case_Folding.Case_Folding_Map;
   State : Containers.Murmur_Hash_3.State :=
      Containers.Murmur_Hash_3.Initialize (0);
   Count : Containers.Count_Type := 0;
   Result : Containers.Hash_Type;
   Last : Natural := Key'First - 1;
begin
   while Last < Key'Last loop
      declare
         Code : Wide_Wide_Character;
         Is_Illegal_Sequence : Boolean; -- ignore
      begin
         --  get single unicode character
         Get (
            Key (Last + 1 .. Key'Last),
            Last,
            Code,
            Is_Illegal_Sequence);
         Count := Count + 1;
         --  update
         Containers.Murmur_Hash_3.Update (
            State,
            Containers.Hash_Type'(
               Wide_Wide_Character'Pos (
                  Strings.Naked_Maps.Value (Mapping.all, Code))));
      end;
   end loop;
   Containers.Murmur_Hash_3.Update (State, Count);
   Containers.Murmur_Hash_3.Finalize (State, Result);
   return Result;
end Ada.Strings.Generic_Hash_Case_Insensitive;
