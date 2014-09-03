package body Ada.Characters.Inside.Sets is

   procedure Append (
      Target : in out Character_Ranges;
      Last : in out Natural;
      Item : Character_Range);
   procedure Append (
      Target : in out Character_Ranges;
      Last : in out Natural;
      Item : Character_Range) is
   begin
      if Last >= Target'First
         and then Target (Last).High >= Wide_Wide_Character'Pred (Item.Low)
      then
         if Item.High > Target (Last).High then
            Target (Last).High := Item.High;
         end if;
      else
         Last := Last + 1;
         Target (Last) := Item;
      end if;
   end Append;

   function Search (A : Character_Ranges; L, H : Character_Type)
      return Positive;
   function Search (A : Character_Ranges; L, H : Character_Type)
      return Positive
   is
      First : Positive := A'First;
      Last : Natural := A'Last;
   begin
      loop
         if First > Last then
            return First; -- return the insertion position when not found
         else
            declare
               Middle : constant Integer := (First + Last) / 2;
            begin
               if H < A (Middle).Low then
                  Last := Middle - 1;
               elsif L > A (Middle).High then
                  First := Middle + 1;
               else
                  return Middle;
               end if;
            end;
         end if;
      end loop;
   end Search;

   --  implementation

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
               pragma Assert (L > A (Last).High);
               if L = Wide_Wide_Character'Succ (A (Last).High) then
                  A (Last).High := H;
               else
                  Last := Last + 1;
                  A (Last).Low := L;
                  A (Last).High := H;
               end if;
            elsif (Index = A'First
                  or else L > Wide_Wide_Character'Succ (A (Index - 1).High))
               and then H < Wide_Wide_Character'Pred (A (Index).Low)
            then
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

   function Is_In (
      Element : Character_Type;
      Set : Character_Set)
      return Boolean
   is
      Index : constant Integer := Search (Set.Items, Element, Element);
   begin
      return Index <= Set.Items'Last
         and then Element >= Set.Items (Index).Low
         and then Element <= Set.Items (Index).High;
   end Is_In;

   function Is_In (
      Element : Character;
      Set : Character_Set)
      return Boolean is
   begin
      return Is_In (To_Wide_Wide_Character (Element), Set);
   end Is_In;

   procedure Merge (
      Target : out Character_Ranges;
      Last : out Natural;
      Left, Right : Character_Ranges)
   is
      I : Positive := Left'First;
      J : Positive := Right'First;
   begin
      Last := Target'First - 1;
      loop
         if J > Right'Last then
            while I <= Left'Last loop
               Append (Target, Last, Left (I));
               I := I + 1;
            end loop;
            exit;
         elsif I > Left'Last then
            while J <= Right'Last loop
               Append (Target, Last, Right (J));
               J := J + 1;
            end loop;
            exit;
         elsif Left (I).Low < Right (J).Low then
            Append (Target, Last, Left (I));
            I := I + 1;
         else
            Append (Target, Last, Right (J));
            J := J + 1;
         end if;
      end loop;
   end Merge;

   procedure Merge (
      Target : out Character_Ranges;
      Last : out Natural;
      Source : Character_Ranges_Array)
   is
      Live_Source : Character_Ranges_Array := Source; -- for modifying
      Live_Source_Indexes : array (Live_Source'Range) of Positive;
      Live_Source_Last : Natural := Live_Source'Last;
   begin
      for I in Live_Source'Range loop
         Live_Source_Indexes (I) := Live_Source (I).all'First;
      end loop;
      Last := Target'First - 1;
      while Live_Source_Last >= Live_Source'First loop
         declare
            Min_Index : Positive;
         begin
            --  select the lowest range
            Min_Index := Live_Source'First;
            for I in Live_Source'First + 1 .. Live_Source_Last loop
               if Live_Source (I).all (Live_Source_Indexes (I)).Low <
                  Live_Source (Min_Index).all (
                     Live_Source_Indexes (Min_Index)).Low
               then
                  Min_Index := I;
               end if;
            end loop;
            --  merge the lowest range
            Append (
               Target,
               Last,
               Live_Source (Min_Index).all (Live_Source_Indexes (Min_Index)));
            --  increment the index
            Live_Source_Indexes (Min_Index) :=
               Live_Source_Indexes (Min_Index) + 1;
            if Live_Source_Indexes (Min_Index) >
               Live_Source (Min_Index).all'Last
            then
               --  remove the finished source
               Live_Source (Min_Index) := Live_Source (Live_Source_Last);
               Live_Source_Indexes (Min_Index) :=
                  Live_Source_Indexes (Live_Source_Last);
               Live_Source_Last := Live_Source_Last - 1;
            end if;
         end;
      end loop;
   end Merge;

end Ada.Characters.Inside.Sets;
