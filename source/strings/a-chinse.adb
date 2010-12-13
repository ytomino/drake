package body Ada.Characters.Inside.Sets is

   procedure Add (
      A : in out Character_Ranges;
      Last : in out Natural;
      L, H : Character_Type) is
   begin
      if Last < A'First then
         Last := Last + 1;
         A (Last).Low := L;
         A (Last).High := H;
      else
         declare
            Index : Positive := Search (A (A'First .. Last), L, H);
            J : Positive;
            New_Last : Natural;
         begin
            if Index > Last then
               --  append to last
               Last := Last + 1;
               A (Last).Low := L;
               A (Last).High := H;
            elsif H < Wide_Wide_Character'Pred (A (Index).Low) then
               --  insert
               New_Last := Last + 1;
               A (A'First + 1 .. New_Last) := A (A'First .. Last);
               Last := New_Last;
               A (Index).Low := L;
               A (Index).High := H;
            else
               --  back while includable
               while Index > A'First
                  and then Wide_Wide_Character'Succ (A (Index - 1).High) >= L
               loop
                  Index := Index - 1;
               end loop;
               --  front while includable
               J := Index;
               while J < Last
                  and then Wide_Wide_Character'Pred (A (J + 1).Low) <= H
               loop
                  J := J + 1;
               end loop;
               --  merge
               A (Index).Low := Wide_Wide_Character'Min (A (Index).Low, L);
               A (Index).High := Wide_Wide_Character'Max (A (J).High, H);
               --  remove merged
               if J > Index then
                  New_Last := Last - (J - Index);
                  A (Index + 1 .. New_Last) := A (J + 1 .. Last);
                  Last := New_Last;
               end if;
            end if;
         end;
      end if;
   end Add;

   function Is_In (Element : Character_Type; Set : Character_Set)
      return Boolean
   is
      Index : constant Integer := Search (Set.Items, Element, Element);
   begin
      return Index < Set.Items'Last
         and then Element >= Set.Items (Index).Low
         and then Element <= Set.Items (Index).High;
   end Is_In;

   function Search (A : Character_Ranges; L, H : Character_Type)
      return Positive is
   begin
      if A'First > A'Last then
         return A'First;
      else
         declare
            Middle : constant Integer := (A'First + A'Last) / 2;
         begin
            if H < A (Middle).Low then
               return Search (A (A'First .. Middle - 1), L, H);
            elsif L > A (Middle).High then
               return Search (A (Middle + 1 .. A'Last), L, H);
            else
               return Middle;
            end if;
         end;
      end if;
   end Search;

end Ada.Characters.Inside.Sets;
