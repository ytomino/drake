package body System.Form_Parameters is

   --  parsing form parameter

   procedure Form_Parameter (
      Form : String;
      Keyword : String;
      First : out Positive;
      Last : out Natural)
   is
      Max_Last : constant Integer := Form'Last - (Keyword'Length - 1);
      I : Positive := Form'First;
   begin
      while I <= Max_Last loop
         declare
            Next : Positive := I;
         begin
            Get_Keyword : loop
               if Next > Form'Last or else Form (Next) = ',' then
                  if Form (I .. Next - 1) = Keyword then
                     --  keyword is found, but value is empty
                     First := Next;
                     Last := Next - 1;
                     return;
                  else
                     I := Next + 1; -- skip ','
                     exit Get_Keyword;
                  end if;
               elsif Form (Next) = '=' then
                  Next := Next + 1; -- skip '='
                  declare
                     Value_First : constant Positive := Next;
                  begin
                     while Next <= Form'Last and then Form (Next) /= ',' loop
                        Next := Next + 1;
                     end loop;
                     if Form (I .. Value_First - 2) = Keyword then
                        --  keyword is found
                        First := Value_First;
                        Last := Next - 1;
                        return;
                     else
                        --  skip value
                        I := Next + 1; -- skip ','
                        exit Get_Keyword;
                     end if;
                  end;
               else
                  Next := Next + 1;
               end if;
            end loop Get_Keyword;
         end;
      end loop;
      First := Form'First;
      Last := First - 1;
   end Form_Parameter;

end System.Form_Parameters;
