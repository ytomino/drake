with Ada.Unchecked_Deallocation;
package body Ada.Strings.Wide_Wide_Maps is
   use type Interfaces.Integer_32;

   function Valid (Data : Set_Data_Access) return Boolean;
   function Valid (Data : Set_Data_Access) return Boolean is
   begin
      if Data = null then
         return True;
      elsif Data.Length = 0 then
         return False;
      else
         for I in Data.Items'First .. Data.Items'Last loop
            if Data.Items (I).High < Data.Items (I).Low then
               return False;
            end if;
         end loop;
         for I in Data.Items'First .. Data.Items'Last - 1 loop
            if Data.Items (I).High >= Data.Items (I + 1).Low then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Valid;

   procedure Free is new Unchecked_Deallocation (Set_Data, Set_Data_Access);
   procedure Free is new Unchecked_Deallocation (Map_Data, Map_Data_Access);

   function Search (A : Wide_Wide_Character_Ranges; L, H : Wide_Wide_Character)
      return Positive;
   function Search (A : Wide_Wide_Character_Ranges; L, H : Wide_Wide_Character)
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

   procedure Add (
      A : in out Wide_Wide_Character_Ranges;
      Last : in out Natural;
      L, H : Wide_Wide_Character);
   procedure Add (
      A : in out Wide_Wide_Character_Ranges;
      Last : in out Natural;
      L, H : Wide_Wide_Character) is
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

   --  local, "+"/or operation
   procedure Add (
      Result : in out Wide_Wide_Character_Ranges;
      Last : out Natural;
      Left, Right : Wide_Wide_Character_Ranges);
   procedure Add (
      Result : in out Wide_Wide_Character_Ranges;
      Last : out Natural;
      Left, Right : Wide_Wide_Character_Ranges) is
   begin
      --  it should be more optimized...
      Last := Result'First + Left'Length - 1;
      Result (Result'First .. Last) := Left;
      for I in Right'Range loop
         Add (
            Result,
            Last,
            Right (I).Low,
            Right (I).High);
      end loop;
   end Add;

   --  local, "-" operation
   procedure Sub (
      Result : in out Wide_Wide_Character_Ranges;
      Last : out Natural;
      Left, Right : Wide_Wide_Character_Ranges);
   procedure Sub (
      Result : in out Wide_Wide_Character_Ranges;
      Last : out Natural;
      Left, Right : Wide_Wide_Character_Ranges)
   is
      I : Positive := Left'First;
      J : Positive := Right'First;
   begin
      Last := Result'First - 1;
      while I <= Left'Last and then J <= Right'Last loop
         if Left (I).High < Right (I).Low then
            Last := Last + 1;
            Result (Last) := Left (I);
            I := I + 1;
         elsif Left (J).Low > Right (I).High then
            J := J + 1;
         else
            declare
               L : Wide_Wide_Character := Left (I).Low;
            begin
               while L <= Left (I).High and then J <= Right'Last loop
                  if L < Right (J).Low then
                     Last := Last + 1;
                     Result (Last).Low := L;
                     Result (Last).High :=
                        Wide_Wide_Character'Pred (Right (J).Low);
                  end if;
                  L := Wide_Wide_Character'Succ (Right (J).High);
                  if Right (J).High <= Left (I).High then
                     J := J + 1;
                  end if;
               end loop;
               if L <= Left (I).High then
                  Last := Last + 1;
                  Result (Last).Low := L;
                  Result (Last).High := Left (I).High;
               end if;
               I := I + 1;
            end;
         end if;
      end loop;
      --  right is over
      while I <= Left'Last loop
         Last := Last + 1;
         Result (Last) := Left (I);
      end loop;
   end Sub;

   --  local, "*"/and operation
   procedure Mul (
      Result : in out Wide_Wide_Character_Ranges;
      Last : out Natural;
      Left, Right : Wide_Wide_Character_Ranges);
   procedure Mul (
      Result : in out Wide_Wide_Character_Ranges;
      Last : out Natural;
      Left, Right : Wide_Wide_Character_Ranges)
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
   end Mul;

   Full_Set_Data : aliased constant Set_Data := (
      Length => 1,
      Reference_Count => -1, -- -1 as constant
      Items => (1 => (
         Low => Wide_Wide_Character'First,
         High => Wide_Wide_Character'Last)));

   overriding procedure Adjust (Object : in out Wide_Wide_Character_Set) is
   begin
      if Object.Data /= null
         and then Object.Data.Reference_Count /= -1
      then
         Interfaces.sync_add_and_fetch (Object.Data.Reference_Count'Access, 1);
      end if;
   end Adjust;

   overriding procedure Adjust (Object : in out Wide_Wide_Character_Mapping) is
   begin
      if Object.Data /= null
         and then Object.Data.Reference_Count /= -1
      then
         Interfaces.sync_add_and_fetch (Object.Data.Reference_Count'Access, 1);
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Wide_Wide_Character_Set) is
   begin
      if Object.Data /= null
         and then Object.Data.Reference_Count /= -1
      then
         if Interfaces.sync_sub_and_fetch (
            Object.Data.Reference_Count'Access,
            1) = 0
         then
            Free (Object.Data);
         end if;
      end if;
   end Finalize;

   overriding procedure Finalize (
      Object : in out Wide_Wide_Character_Mapping) is
   begin
      if Object.Data /= null
         and then Object.Data.Reference_Count /= -1
      then
         if Interfaces.sync_sub_and_fetch (
            Object.Data.Reference_Count'Access,
            1) = 0
         then
            Free (Object.Data);
         end if;
      end if;
   end Finalize;

   function Identity return Wide_Wide_Character_Mapping is
   begin
      return (Ada.Finalization.Controlled with Data => null);
   end Identity;

   function Is_In (
      Element : Wide_Wide_Character;
      Set : Wide_Wide_Character_Set)
      return Boolean is
   begin
      pragma Assert (Valid (Set.Data));
      if Set.Data = null then
         return False;
      else
         declare
            Index : constant Integer :=
               Search (Set.Data.Items, Element, Element);
         begin
            return Index < Set.Data.Items'Last
               and then Element >= Set.Data.Items (Index).Low
               and then Element <= Set.Data.Items (Index).High;
         end;
      end if;
   end Is_In;

   function Is_Subset (
      Elements : Wide_Wide_Character_Set;
      Set : Wide_Wide_Character_Set)
      return Boolean is
   begin
      pragma Assert (Valid (Elements.Data));
      pragma Assert (Valid (Set.Data));
      if Elements.Data = null then
         return True;
      elsif Set.Data = null then
         return False;
      else
         declare
            J : Positive := Set.Data.Items'First;
         begin
            for I in Elements.Data.Items'Range loop
               declare
                  E : Wide_Wide_Character_Range
                     renames Elements.Data.Items (I);
               begin
                  loop
                     if E.Low < Set.Data.Items (J).Low then
                        return False;
                     elsif E.High > Set.Data.Items (J).High then
                        J := J + 1;
                        if J > Set.Data.Items'Last then
                           return False;
                        end if;
                     else
                        exit; --  ok for E
                     end if;
                  end loop;
               end;
            end loop;
            return True;
         end;
      end if;
   end Is_Subset;

   function Null_Set return Wide_Wide_Character_Set is
   begin
      return (Ada.Finalization.Controlled with Data => null);
   end Null_Set;

   function To_Domain (Map : Wide_Wide_Character_Mapping)
      return Wide_Wide_Character_Sequence is
   begin
      if Map.Data = null then
         return "";
      else
         return Map.Data.From;
      end if;
   end To_Domain;

   function To_Mapping (From, To : Wide_Wide_Character_Sequence)
      return Wide_Wide_Character_Mapping
   is
      New_Data : Map_Data_Access;
   begin
      if From'Length = 0 then
         New_Data := null;
      else
         New_Data := new Map_Data'(Characters.Inside.To_Mapping (
            From,
            To,
            Initial_Reference_Count => 1));
      end if;
      return (Finalization.Controlled with Data => New_Data);
   end To_Mapping;

   function To_Range (Map : Wide_Wide_Character_Mapping)
      return Wide_Wide_Character_Sequence is
   begin
      if Map.Data = null then
         return "";
      else
         return Map.Data.To;
      end if;
   end To_Range;

   function To_Ranges (Set : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Ranges is
   begin
      pragma Assert (Valid (Set.Data));
      if Set.Data = null then
         return Wide_Wide_Character_Ranges'(1 .. 0 => <>); --  empty
      else
         return Set.Data.Items;
      end if;
   end To_Ranges;

   function To_Set (Ranges : Wide_Wide_Character_Ranges)
      return Wide_Wide_Character_Set
   is
      Items : Wide_Wide_Character_Ranges (Ranges'Range);
      Last : Natural := Items'First;
      Data : Set_Data_Access;
   begin
      for I in Ranges'Range loop
         if Ranges (I).Low <= Ranges (I).High then
            Add (Items, Last, Ranges (I).Low, Ranges (I).High);
         end if;
      end loop;
      if Last = 0 then
         Data := null;
      else
         Data := new Set_Data'(
            Length => Items'Length,
            Reference_Count => 1,
            Items => Items);
         pragma Assert (Valid (Data));
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end To_Set;

   function To_Set (Span : Wide_Wide_Character_Range)
      return Wide_Wide_Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Span.Low > Span.High then
         Data := null;
      else
         Data := new Set_Data'(
            Length => 1,
            Reference_Count => 1,
            Items => (1 => Span));
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end To_Set;

   function To_Set (Sequence : Wide_Wide_Character_Sequence)
      return Wide_Wide_Character_Set
   is
      Items : Wide_Wide_Character_Ranges (Sequence'Range);
      Last : Natural := Items'First;
      Data : Set_Data_Access;
   begin
      --  it should be more optimized...
      for I in Sequence'Range loop
         Add (Items, Last, Sequence (I), Sequence (I));
      end loop;
      if Last = 0 then
         Data := null;
      else
         Data := new Set_Data'(
            Length => Items'Length,
            Reference_Count => 1,
            Items => Items);
         pragma Assert (Valid (Data));
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end To_Set;

   function To_Set (Singleton : Wide_Wide_Character)
      return Wide_Wide_Character_Set is
   begin
      return (Ada.Finalization.Controlled with
         Data => new Set_Data'(
            Length => 1,
            Reference_Count => 1,
            Items => (1 => (Low => Singleton, High => Singleton))));
   end To_Set;

   function To_Sequence (Set : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Sequence is
   begin
      pragma Assert (Valid (Set.Data));
      if Set.Data = null then
         return "";
      else
         declare
            Length : Natural := 0;
            Position : Positive;
         begin
            for I in Set.Data.Items'Range loop
               Length := Length +
                  Wide_Wide_Character'Pos (Set.Data.Items (I).High) -
                  Wide_Wide_Character'Pos (Set.Data.Items (I).Low);
            end loop;
            return Result : Wide_Wide_String (1 .. Length) do
               Position := 1;
               for I in Set.Data.Items'Range loop
                  for J in
                     Set.Data.Items (I).Low ..
                     Set.Data.Items (I).High
                  loop
                     Result (Position) := J;
                     Position := Position + 1;
                  end loop;
               end loop;
            end return;
         end;
      end if;
   end To_Sequence;

   function Value (
      Map : Wide_Wide_Character_Mapping;
      Element : Wide_Wide_Character)
      return Wide_Wide_Character is
   begin
      if Map.Data = null then
         return Element;
      else
         return Characters.Inside.Value (Map.Data, Element);
      end if;
   end Value;

   function "=" (Left, Right : Wide_Wide_Character_Set) return Boolean is
   begin
      pragma Assert (Valid (Left.Data));
      pragma Assert (Valid (Right.Data));
      return Left.Data = Right.Data
         or else (Left.Data /= null
            and then Right.Data /= null
            and then Left.Data.Items = Right.Data.Items);
   end "=";

   function "not" (Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Right.Data = null then
         Data := Full_Set_Data'Unrestricted_Access;
      else
         declare
            Items : Wide_Wide_Character_Ranges (
               1 ..
               Full_Set_Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Sub (Items, Last, Full_Set_Data.Items, Right.Data.Items);
            Data := new Set_Data'(
               Length => Items'Length,
               Reference_Count => 1,
               Items => Items);
            pragma Assert (Valid (Data));
         end;
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end "not";

   function "and" (Left, Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Left.Data = null or else Right.Data = null then
         Data := null;
      else
         declare
            Items : Wide_Wide_Character_Ranges (
               1 ..
               Left.Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Mul (Items, Last, Left.Data.Items, Right.Data.Items);
            if Last = 0 then
               Data := null;
            else
               Data := new Set_Data'(
                  Length => Items'Length,
                  Reference_Count => 1,
                  Items => Items);
               pragma Assert (Valid (Data));
            end if;
         end;
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end "and";

   function "or" (Left, Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Left.Data = null and then Right.Data = null then
         Data := null;
      elsif Left.Data = null then
         return Right;
      elsif Right.Data = null then
         return Left;
      else
         declare
            Items : Wide_Wide_Character_Ranges (
               1 ..
               Left.Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Add (Items, Last, Left.Data.Items, Right.Data.Items);
            Data := new Set_Data'(
               Length => Items'Length,
               Reference_Count => 1,
               Items => Items);
            pragma Assert (Valid (Data));
         end;
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end "or";

   function "xor" (Left, Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Left.Data = null and then Right.Data = null then
         Data := null;
      elsif Left.Data = null then
         return Right;
      elsif Right.Data = null then
         return Left;
      else
         declare
            Max : constant Natural := Left.Data.Length + Right.Data.Length;
            X : Wide_Wide_Character_Ranges (1 .. Max);
            X_Last : Natural;
            Y : Wide_Wide_Character_Ranges (1 .. Max);
            Y_Last : Natural;
            Items : Wide_Wide_Character_Ranges (1 .. Max);
            Last : Natural;
         begin
            Add (X, X_Last, Left.Data.Items, Right.Data.Items);
            Mul (Y, Y_Last, Left.Data.Items, Right.Data.Items);
            Sub (Items, Last, X (1 .. X_Last), Y (1 .. Y_Last));
            if Last = 0 then
               Data := null;
            else
               Data := new Set_Data'(
                  Length => Items'Length,
                  Reference_Count => 1,
                  Items => Items);
               pragma Assert (Valid (Data));
            end if;
         end;
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end "xor";

   function "-" (Left, Right : Wide_Wide_Character_Set)
      return Wide_Wide_Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Left.Data = null then
         Data := null;
      elsif Right.Data = null then
         return Left;
      else
         declare
            Items : Wide_Wide_Character_Ranges (
               1 ..
               Left.Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Sub (Items, Last, Left.Data.Items, Right.Data.Items);
            if Last = 0 then
               Data := null;
            else
               Data := new Set_Data'(
                  Length => Items'Length,
                  Reference_Count => 1,
                  Items => Items);
               pragma Assert (Valid (Data));
            end if;
         end;
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end "-";

end Ada.Strings.Wide_Wide_Maps;
