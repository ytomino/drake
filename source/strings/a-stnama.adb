with Ada.Characters.Conversions;
with Ada.Exception_Identification.From_Here;
package body Ada.Strings.Naked_Maps is
   use Exception_Identification.From_Here;

   type Long_Boolean is new Boolean;
   for Long_Boolean'Size use Long_Integer'Size;

   function expect (exp, c : Long_Boolean) return Long_Boolean
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_expect";

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

   function To_Wide_Character (Item : Wide_Wide_Character)
      return Wide_Character is
   begin
      if Characters.Conversions.Is_Wide_Character (Item) then
         return Wide_Character'Val (Wide_Wide_Character'Pos (Item));
      else
         raise Constraint_Error;
      end if;
   end To_Wide_Character;

   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character is
   begin
      if Characters.Conversions.Is_Wide_Wide_Character (Item) then
         return Wide_Wide_Character'Val (Character'Pos (Item));
      else
         raise Constraint_Error;
      end if;
   end To_Wide_Wide_Character;

   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character is
   begin
      if Characters.Conversions.Is_Wide_Wide_Character (Item) then
         return Wide_Wide_Character'Val (Wide_Character'Pos (Item));
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
      while First <= Last loop
         declare
            type Unsigned is mod 2 ** Integer'Size;
            Middle : constant Positive :=
               Integer (Unsigned (First + Last) / 2);
            Middle_Item : Character_Range
               renames A (Middle);
         begin
            if H < Middle_Item.Low then
               Last := Middle - 1;
            elsif expect (Long_Boolean (L > Middle_Item.High), True) then
               First := Middle + 1;
            else
               return Middle;
            end if;
         end;
      end loop;
      return First; -- the insertion position when not found
   end Search;

   --  implementation of sets

   function Is_In (Element : Character_Type; Set : Character_Set_Data)
      return Boolean
   is
      Index : constant Integer := Search (Set.Items, Element, Element);
   begin
      return Index <= Set.Items'Last
         and then Element >= Set.Items (Index).Low
         and then Element <= Set.Items (Index).High;
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

   procedure Intersection (
      Result : out Character_Ranges;
      Last : out Natural;
      Left, Right : Character_Ranges)
   is
      I : Positive := Left'First;
      J : Positive := Right'First;
   begin
      Last := Result'First - 1;
      while I <= Left'Last and then J <= Right'Last loop
         if Left (I).High < Right (J).Low then
            I := I + 1;
         elsif Right (J).High < Left (I).Low then
            J := J + 1;
         else
            Last := Last + 1;
            Result (Last).Low :=
               Wide_Wide_Character'Max (Left (I).Low, Right (J).Low);
            Result (Last).High :=
               Wide_Wide_Character'Min (Left (I).High, Right (J).High);
            if Left (I).High < Right (J).High then
               I := I + 1;
            else
               J := J + 1;
            end if;
         end if;
      end loop;
   end Intersection;

   procedure Union (
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
   end Union;

   procedure Union (
      Target : out Character_Ranges;
      Last : out Natural;
      Source : in out Character_Set_Array)
   is
      Source_Indexes : array (Source'Range) of Positive := (others => 1);
      Source_Last : Natural := Source'Last;
   begin
      Last := Target'First - 1;
      while Source_Last >= Source'First loop
         declare
            Min_Index : Positive;
         begin
            --  select the lowest range
            Min_Index := Source'First;
            for I in Source'First + 1 .. Source_Last loop
               if Source (I).Items (Source_Indexes (I)).Low <
                  Source (Min_Index).Items (Source_Indexes (Min_Index)).Low
               then
                  Min_Index := I;
               end if;
            end loop;
            --  merge the lowest range
            Append (
               Target,
               Last,
               Source (Min_Index).Items (Source_Indexes (Min_Index)));
            --  increment the index
            Source_Indexes (Min_Index) := Source_Indexes (Min_Index) + 1;
            if Source_Indexes (Min_Index) > Source (Min_Index).Length then
               --  remove the finished source
               Source (Min_Index) := Source (Source_Last);
               Source_Indexes (Min_Index) := Source_Indexes (Source_Last);
               Source_Last := Source_Last - 1;
            end if;
         end;
      end loop;
   end Union;

   --  implementation of maps

   function Value (Map : Character_Mapping_Data; Element : Character_Type)
      return Character_Type
   is
      L : Positive := Map.From'First;
      H : Natural := Map.From'Last;
   begin
      while L <= H loop
         declare
            type Unsigned is mod 2 ** Integer'Size;
            M : constant Positive := Integer (Unsigned (L + H) / 2);
            M_Key : constant Wide_Wide_Character := Map.From (M);
         begin
            if Element < M_Key then
               H := M - 1;
            elsif expect (Long_Boolean (Element > M_Key), True) then
               L := M + 1;
            else
               return Map.To (M);
            end if;
         end;
      end loop;
      return Element;
   end Value;

   function Translate (
      Source : String;
      Mapping : Character_Mapping_Data)
      return String
   is
      Result : String (
         1 ..
         Source'Length * Characters.Conversions.Max_Length_In_String);
      Source_Last : Natural := Source'First - 1;
      Result_Last : Natural := Result'First - 1;
   begin
      while Source_Last < Source'Last loop
         declare
            Source_Index : constant Positive := Source_Last + 1;
            Index : constant Positive := Result_Last + 1;
            Code : Wide_Wide_Character;
            Is_Illegal_Sequence : Boolean;
         begin
            --  get single unicode character
            Characters.Conversions.Get (
               Source (Source_Index .. Source'Last),
               Source_Last,
               Code,
               Is_Illegal_Sequence => Is_Illegal_Sequence);
            if Is_Illegal_Sequence then
               --  keep illegal sequence
               Result_Last := Index + (Source_Last - Source_Index);
               Result (Index .. Result_Last) :=
                  Source (Source_Index .. Source_Last);
            else
               --  map it
               Code := Value (Mapping, Code);
               --  put it
               Characters.Conversions.Put (
                  Code,
                  Result (Index .. Result'Last),
                  Result_Last);
            end if;
         end;
      end loop;
      return Result (1 .. Result_Last);
   end Translate;

   function Translate (
      Source : Wide_String;
      Mapping : Character_Mapping_Data)
      return Wide_String
   is
      Result : Wide_String (
         1 ..
         Source'Length * Characters.Conversions.Max_Length_In_Wide_String);
      Source_Last : Natural := Source'First - 1;
      Result_Last : Natural := Result'First - 1;
   begin
      while Source_Last < Source'Last loop
         declare
            Source_Index : constant Positive := Source_Last + 1;
            Index : constant Positive := Result_Last + 1;
            Code : Wide_Wide_Character;
            Is_Illegal_Sequence : Boolean;
         begin
            --  get single unicode character
            Characters.Conversions.Get (
               Source (Source_Index .. Source'Last),
               Source_Last,
               Code,
               Is_Illegal_Sequence => Is_Illegal_Sequence);
            if Is_Illegal_Sequence then
               --  keep illegal sequence
               Result_Last := Index + (Source_Last - Source_Index);
               Result (Index .. Result_Last) :=
                  Source (Source_Index .. Source_Last);
            else
               --  map it
               Code := Value (Mapping, Code);
               --  put it
               Characters.Conversions.Put (
                  Code,
                  Result (Index .. Result'Last),
                  Result_Last);
            end if;
         end;
      end loop;
      return Result (1 .. Result_Last);
   end Translate;

   function Translate (
      Source : Wide_Wide_String;
      Mapping : Character_Mapping_Data)
      return Wide_Wide_String
   is
      Result : Wide_Wide_String (1 .. Source'Length);
      Source_Last : Natural := Source'First - 1;
      Result_Last : Natural := Result'First - 1;
   begin
      while Source_Last < Source'Last loop
         declare
            Source_Index : constant Positive := Source_Last + 1;
            Index : constant Positive := Result_Last + 1;
            Code : Wide_Wide_Character;
            Is_Illegal_Sequence : Boolean;
         begin
            --  get single unicode character
            Characters.Conversions.Get (
               Source (Source_Index .. Source'Last),
               Source_Last,
               Code,
               Is_Illegal_Sequence => Is_Illegal_Sequence);
            if Is_Illegal_Sequence then
               --  keep illegal sequence
               Result_Last := Index + (Source_Last - Source_Index);
               Result (Index .. Result_Last) :=
                  Source (Source_Index .. Source_Last);
            else
               --  map it
               Code := Value (Mapping, Code);
               --  put it
               Characters.Conversions.Put (
                  Code,
                  Result (Index .. Result'Last),
                  Result_Last);
            end if;
         end;
      end loop;
      return Result (1 .. Result_Last);
   end Translate;

   procedure To_Mapping (
      From, To : Character_Sequence;
      Out_From, Out_To : out Character_Sequence; -- should have same 'First
      Out_Last : out Natural)
   is
      From_Length : constant Natural := From'Length;
   begin
      if From_Length /= To'Length then
         Raise_Exception (Translation_Error'Identity);
      else
         pragma Assert (Out_From'First = Out_To'First);
         declare
            Out_First : constant Positive := Out_From'First;
         begin
            Out_Last := Out_First + From_Length - 1;
            Out_From (Out_First .. Out_Last) := From;
            Out_To (Out_First .. Out_Last) := To;
            for I in reverse Out_First .. Out_Last loop
               if Out_From (I) = Out_To (I) then
                  Out_From (I) := Out_From (Out_Last);
                  Out_To (I) := Out_To (Out_Last);
                  Out_Last := Out_Last - 1;
               end if;
            end loop;
            Sort (
               From => Out_From (Out_First .. Out_Last),
               To => Out_To (Out_First .. Out_Last));
         end;
      end if;
   end To_Mapping;

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

end Ada.Strings.Naked_Maps;
