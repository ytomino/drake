with Ada.Characters.Conversions;
with Ada.Exception_Identification.From_Here;
with System.UTF_Conversions;
package body Ada.Strings.Naked_Maps is
   use Exception_Identification.From_Here;
   use type System.UTF_Conversions.From_Status_Type;
   use type System.UTF_Conversions.UCS_4;

   --  implementation of alternative conversions functions

   function To_Character (Item : Wide_Wide_Character)
      return Character is
   begin
      if Characters.Conversions.Is_Character (Item) then
         return Character'Val (Wide_Wide_Character'Pos (Item));
      else
         raise Constraint_Error;
      end if;
   end To_Character;

   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character is
   begin
      if Characters.Conversions.Is_Wide_Wide_Character (Item) then
         return Wide_Wide_Character'Val (Character'Pos (Item));
      else
         raise Constraint_Error;
      end if;
   end To_Wide_Wide_Character;

   --  sets

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

   --  implementation of sets

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

   --  implementation of maps

   function To_Mapping (
      From, To : Character_Sequence;
      Initial_Reference_Count : System.Reference_Counting.Counter)
      return Character_Mapping
   is
      From_Length : constant Natural := From'Length;
   begin
      if From_Length /= To'Length then
         Raise_Exception (Translation_Error'Identity);
      else
         declare
            Sorted_From : Character_Sequence (1 .. From_Length) := From;
            Sorted_To : Character_Sequence (1 .. From_Length) := To;
            Last : Natural;
         begin
            Sort (Sorted_From, Sorted_To, Last);
            return Character_Mapping'(
               Length => Last,
               Reference_Count => Initial_Reference_Count,
               From => Sorted_From (1 .. Last),
               To => Sorted_To (1 .. Last));
         end;
      end if;
   end To_Mapping;

   function Value (
      Map : Character_Mapping;
      Element : Character_Type)
      return Character_Type
   is
      L : Positive := Map.From'First;
      H : Natural := Map.From'Last;
   begin
      loop
         exit when L > H;
         declare
            M : constant Positive := (L + H) / 2;
         begin
            if Element < Map.From (M) then
               H := M - 1;
            elsif Element > Map.From (M) then
               L := M + 1;
            else
               return Map.To (M);
            end if;
         end;
      end loop;
      return Element;
   end Value;

   function Value (
      Map : Character_Mapping;
      Element : Character)
      return Character is
   begin
      return To_Character (Value (Map, To_Wide_Wide_Character (Element)));
   end Value;

   procedure Translate (
      Source : String;
      Mapping : Character_Mapping;
      Item : out String; -- Source'Length * 6, at least
      Last : out Natural)
   is
      Source_Last : Natural := Source'First - 1;
   begin
      Last := Item'First - 1;
      while Source_Last < Source'Last loop
         declare
            Source_Index : constant Positive := Source_Last + 1;
            Index : constant Positive := Last + 1;
            Code : System.UTF_Conversions.UCS_4;
            From_Status : System.UTF_Conversions.From_Status_Type;
            To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
         begin
            --  get single unicode character
            System.UTF_Conversions.From_UTF_8 (
               Source (Source_Index .. Source'Last),
               Source_Last,
               Code,
               From_Status);
            if From_Status = System.UTF_Conversions.Success then
               --  map it
               Code := Wide_Wide_Character'Pos (
                  Value (Mapping, Wide_Wide_Character'Val (Code)));
               --  put it
               System.UTF_Conversions.To_UTF_8 (
                  Code,
                  Item (Index .. Item'Last),
                  Last,
                  To_Status);
            else
               --  keep illegal sequence
               Last := Index + (Source_Last - Source_Index);
               Item (Index .. Last) := Source (Source_Index .. Source_Last);
            end if;
         end;
      end loop;
   end Translate;

   procedure Sort (From, To : in out Character_Sequence) is
      pragma Assert (From'First = To'First);
   begin
      for I in From'First + 1 .. From'Last loop
         for J in reverse From'First .. I - 1 loop
            declare
               Temp_F : Character_Type;
               Temp_T : Character_Type;
               K : constant Positive := J + 1;
            begin
               if From (J) = From (K) then
                  Raise_Exception (Translation_Error'Identity);
               end if;
               exit when From (J) <= From (K);
               Temp_F := From (J);
               Temp_T := To (J);
               From (J) := From (K);
               To (J) := To (K);
               From (K) := Temp_F;
               To (K) := Temp_T;
            end;
         end loop;
      end loop;
   end Sort;

   procedure Sort (From, To : in out Character_Sequence; Last : out Natural) is
      pragma Assert (From'First = To'First);
   begin
      Last := From'Last;
      for I in reverse From'Range loop
         if From (I) = To (I) then
            From (I) := From (Last);
            To (I) := To (Last);
            Last := Last - 1;
         end if;
      end loop;
      Sort (From (From'First .. Last), To (To'First .. Last));
   end Sort;

end Ada.Strings.Naked_Maps;
