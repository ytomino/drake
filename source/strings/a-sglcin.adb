with Ada.Strings.Naked_Maps.Case_Folding;
function Ada.Strings.Generic_Less_Case_Insensitive (Left, Right : String_Type)
   return Boolean
is
   Mapping : constant not null access Naked_Maps.Character_Mapping :=
      Strings.Naked_Maps.Case_Folding.Case_Folding_Map;
   Left_Last : Natural := Left'First - 1;
   Right_Last : Natural := Right'First - 1;
begin
   while Left_Last < Left'Last and then Right_Last < Right'Last loop
      declare
         Left_Index : constant Positive := Left_Last + 1;
         Left_Code : Wide_Wide_Character;
         Left_Is_Illegal_Sequence : Boolean;
         Right_Index : constant Positive := Right_Last + 1;
         Right_Code : Wide_Wide_Character;
         Right_Is_Illegal_Sequence : Boolean;
      begin
         Get (
            Left (Left_Index .. Left'Last),
            Left_Last,
            Left_Code,
            Left_Is_Illegal_Sequence);
         Get (
            Right (Right_Index .. Right'Last),
            Right_Last,
            Right_Code,
            Right_Is_Illegal_Sequence);
         if not Left_Is_Illegal_Sequence then
            if not Right_Is_Illegal_Sequence then
               --  Left and Right are legal
               Left_Code := Naked_Maps.Value (Mapping.all, Left_Code);
               Right_Code := Naked_Maps.Value (Mapping.all, Right_Code);
               if Left_Code < Right_Code then
                  return True;
               elsif Left_Code > Right_Code then
                  return False;
               end if;
            else
               --  Left is legal, Right is illegal
               return True; -- legal < illegal
            end if;
         else
            if not Right_Is_Illegal_Sequence then
               --  Left is illegal, Right is legal
               return False; -- illegal > legal
            else
               --  Left and Right are illegal
               declare
                  Left_Seq : String_Type
                     renames Left (Left_Index .. Left_Last);
                  Right_Seq : String_Type
                     renames Right (Right_Index .. Right_Last);
               begin
                  if Left_Seq < Right_Seq then
                     return True;
                  elsif Left_Seq > Right_Seq then
                     return False;
                  end if;
               end;
            end if;
         end if;
      end;
   end loop;
   return (Left_Last >= Left'Last) and then (Right_Last < Right'Last);
end Ada.Strings.Generic_Less_Case_Insensitive;
