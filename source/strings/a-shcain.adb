with Ada.Strings.Naked_Maps.Case_Folding;
with System.UTF_Conversions;
function Ada.Strings.Hash_Case_Insensitive (Key : String)
   return Containers.Hash_Type
is
   use type Containers.Hash_Type;
   Result : Containers.Hash_Type := 0;
   I : Natural := Key'First;
begin
   while I <= Key'Last loop
      declare
         Code : System.UTF_Conversions.UCS_4;
         Next : Natural;
         From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
      begin
         --  get single unicode character
         System.UTF_Conversions.From_UTF_8 (
            Key (I .. Key'Last),
            Next,
            Code,
            From_Status);
         I := Next + 1;
         --  update
         Result := Containers.Rotate_Left (Result, 5)
            xor Wide_Wide_Character'Pos (
               Strings.Naked_Maps.Value (
                  Strings.Naked_Maps.Case_Folding.Case_Folding_Map.all,
                  Wide_Wide_Character'Val (Code)));
      end;
   end loop;
   return Result;
end Ada.Strings.Hash_Case_Insensitive;
