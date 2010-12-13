with Ada.Unchecked_Deallocation;
with Interfaces;
package body Ada.Strings.Wide_Wide_Maps is
   use type Characters.Inside.Sets.Character_Ranges;
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
            if Data.Items (I).High >=
               Wide_Wide_Character'Pred (Data.Items (I + 1).Low)
            then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Valid;

   procedure Free is new Unchecked_Deallocation (Set_Data, Set_Data_Access);
   procedure Free is new Unchecked_Deallocation (Map_Data, Map_Data_Access);

   --  local, "+"/or operation
   procedure Add (
      Result : in out Characters.Inside.Sets.Character_Ranges;
      Last : out Natural;
      Left, Right : Characters.Inside.Sets.Character_Ranges);
   procedure Add (
      Result : in out Characters.Inside.Sets.Character_Ranges;
      Last : out Natural;
      Left, Right : Characters.Inside.Sets.Character_Ranges) is
   begin
      --  it should be more optimized...
      Last := Result'First + Left'Length - 1;
      Result (Result'First .. Last) := Left;
      for I in Right'Range loop
         Characters.Inside.Sets.Add (
            Result,
            Last,
            Right (I).Low,
            Right (I).High);
      end loop;
   end Add;

   --  local, "-" operation
   procedure Sub (
      Result : in out Characters.Inside.Sets.Character_Ranges;
      Last : out Natural;
      Left, Right : Characters.Inside.Sets.Character_Ranges);
   procedure Sub (
      Result : in out Characters.Inside.Sets.Character_Ranges;
      Last : out Natural;
      Left, Right : Characters.Inside.Sets.Character_Ranges)
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
      Result : in out Characters.Inside.Sets.Character_Ranges;
      Last : out Natural;
      Left, Right : Characters.Inside.Sets.Character_Ranges);
   procedure Mul (
      Result : in out Characters.Inside.Sets.Character_Ranges;
      Last : out Natural;
      Left, Right : Characters.Inside.Sets.Character_Ranges)
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
         return Characters.Inside.Sets.Is_In (Element, Set.Data.all);
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
                  E : Characters.Inside.Sets.Character_Range
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
         New_Data := new Map_Data'(Characters.Inside.Maps.To_Mapping (
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
         return Result : Wide_Wide_Character_Ranges (Set.Data.Items'Range) do
            for I in Result'Range loop
               Result (I).Low := Set.Data.Items (I).Low;
               Result (I).High := Set.Data.Items (I).High;
            end loop;
         end return;
      end if;
   end To_Ranges;

   function To_Set (Ranges : Wide_Wide_Character_Ranges)
      return Wide_Wide_Character_Set
   is
      Items : Characters.Inside.Sets.Character_Ranges (1 .. Ranges'Length);
      Last : Natural := Items'First - 1;
      Data : Set_Data_Access;
   begin
      for I in Ranges'Range loop
         if Ranges (I).Low <= Ranges (I).High then
            Characters.Inside.Sets.Add (
               Items,
               Last,
               Ranges (I).Low,
               Ranges (I).High);
         end if;
      end loop;
      if Last < Items'First then
         Data := null;
      else
         Data := new Set_Data'(
            Length => Last - Items'First + 1,
            Reference_Count => 1,
            Items => <>);
         for I in Data.Items'Range loop
            Data.Items (I).Low := Items (I - Items'First + 1).Low;
            Data.Items (I).High := Items (I - Items'First + 1).High;
         end loop;
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
            Items => <>);
         Data.Items (Data.Items'First).Low := Span.Low;
         Data.Items (Data.Items'First).High := Span.High;
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end To_Set;

   function To_Set (Sequence : Wide_Wide_Character_Sequence)
      return Wide_Wide_Character_Set
   is
      Items : Characters.Inside.Sets.Character_Ranges (Sequence'Range);
      Last : Natural := Items'First;
      Data : Set_Data_Access;
   begin
      --  it should be more optimized...
      for I in Sequence'Range loop
         Characters.Inside.Sets.Add (Items, Last, Sequence (I), Sequence (I));
      end loop;
      if Last < Items'First then
         Data := null;
      else
         Data := new Set_Data'(
            Length => Last - Items'First + 1,
            Reference_Count => 1,
            Items => Items (Items'First .. Last));
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
                  Wide_Wide_Character'Pos (Set.Data.Items (I).Low) + 1;
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
         return Characters.Inside.Maps.Value (Map.Data, Element);
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

   function "=" (Left, Right : Wide_Wide_Character_Mapping) return Boolean is
   begin
      return Left.Data = Right.Data
         or else (Left.Data /= null
            and then Right.Data /= null
            and then Left.Data.From = Right.Data.From
            and then Left.Data.To = Right.Data.To);
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
            Items : Characters.Inside.Sets.Character_Ranges (
               1 ..
               Full_Set_Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Sub (Items, Last, Full_Set_Data.Items, Right.Data.Items);
            if Last < Items'First then
               Data := null;
            else
               Data := new Set_Data'(
                  Length => Last - Items'First + 1,
                  Reference_Count => 1,
                  Items => Items (Items'First .. Last));
               pragma Assert (Valid (Data));
            end if;
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
            Items : Characters.Inside.Sets.Character_Ranges (
               1 ..
               Left.Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Mul (Items, Last, Left.Data.Items, Right.Data.Items);
            if Last < Items'First then
               Data := null;
            else
               Data := new Set_Data'(
                  Length => Last - Items'First + 1,
                  Reference_Count => 1,
                  Items => Items (Items'First .. Last));
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
            Items : Characters.Inside.Sets.Character_Ranges (
               1 ..
               Left.Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Add (Items, Last, Left.Data.Items, Right.Data.Items);
            Data := new Set_Data'(
               Length => Last - Items'First + 1,
               Reference_Count => 1,
               Items => Items (Items'First .. Last));
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
            X : Characters.Inside.Sets.Character_Ranges (1 .. Max);
            X_Last : Natural;
            Y : Characters.Inside.Sets.Character_Ranges (1 .. Max);
            Y_Last : Natural;
            Items : Characters.Inside.Sets.Character_Ranges (1 .. Max);
            Last : Natural;
         begin
            Add (X, X_Last, Left.Data.Items, Right.Data.Items);
            Mul (Y, Y_Last, Left.Data.Items, Right.Data.Items);
            Sub (Items, Last, X (1 .. X_Last), Y (1 .. Y_Last));
            if Last < Items'First then
               Data := null;
            else
               Data := new Set_Data'(
                  Length => Last - Items'First + 1,
                  Reference_Count => 1,
                  Items => Items (Items'First .. Last));
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
            Items : Characters.Inside.Sets.Character_Ranges (
               1 ..
               Left.Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Sub (Items, Last, Left.Data.Items, Right.Data.Items);
            if Last < Items'First then
               Data := null;
            else
               Data := new Set_Data'(
                  Length => Last - Items'Length + 1,
                  Reference_Count => 1,
                  Items => Items (Items'First .. Last));
               pragma Assert (Valid (Data));
            end if;
         end;
      end if;
      return (Ada.Finalization.Controlled with Data => Data);
   end "-";

end Ada.Strings.Wide_Wide_Maps;
