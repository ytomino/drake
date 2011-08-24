with Ada.Characters.Inside;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Strings.Stream_Ops;
with System.UTF_Conversions.From_8_To_32;
with System.UTF_Conversions.From_16_To_32;
with System.UTF_Conversions.From_32_To_8;
with System.UTF_Conversions.From_32_To_16;
package body Ada.Strings.Maps is
   pragma Suppress (All_Checks);
   use type Characters.Inside.Sets.Character_Ranges;

   --  sets

   function Valid (Data : not null Set_Data_Access) return Boolean;
   function Valid (Data : not null Set_Data_Access) return Boolean is
   begin
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
   end Valid;

   procedure Free is new Unchecked_Deallocation (Set_Data, Set_Data_Access);

   procedure Free_Set_Data (Data : System.Address);
   procedure Free_Set_Data (Data : System.Address) is
      package Conv is new System.Address_To_Named_Access_Conversions (
         Set_Data,
         Set_Data_Access);
      X : Set_Data_Access := Conv.To_Pointer (Data);
   begin
      Free (X);
   end Free_Set_Data;

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
         I := I + 1;
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
      Reference_Count => System.Reference_Counting.Static,
      Items => (1 => (
         Low => Wide_Wide_Character'First,
         High => Wide_Wide_Character'Last)));

   overriding procedure Adjust (Object : in out Character_Set) is
   begin
      System.Reference_Counting.Adjust (Object.Data.Reference_Count'Access);
   end Adjust;

   overriding procedure Finalize (Object : in out Character_Set) is
      subtype Not_Null_Set_Data_Access is not null Set_Data_Access;
      type Set_Data_Access_Access is access all Not_Null_Set_Data_Access;
      type System_Address_Access is access all System.Address;
      function Upcast is new Unchecked_Conversion (
         Set_Data_Access_Access,
         System_Address_Access);
   begin
      System.Reference_Counting.Clear (
         Upcast (Object.Data'Access),
         Object.Data.Reference_Count'Access,
         Free => Free_Set_Data'Access);
   end Finalize;

   function Is_Null (Set : Character_Set) return Boolean is
   begin
      return Set.Data.Length = 0;
   end Is_Null;

   function Is_Subset (
      Elements : Character_Set;
      Set : Character_Set)
      return Boolean is
   begin
      pragma Assert (Valid (Elements.Data));
      pragma Assert (Valid (Set.Data));
      if Set.Data.Length = 0 then
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

   function Null_Set return Character_Set is
   begin
      return (Finalization.Controlled with
         Data => Empty_Set_Data'Unrestricted_Access);
   end Null_Set;

   function Overloaded_Is_In (
      Element : Character;
      Set : Character_Set)
      return Boolean is
   begin
      return Overloaded_Is_In (
         Characters.Inside.To_Wide_Wide_Character (Element),
         Set);
   end Overloaded_Is_In;

   function Overloaded_Is_In (
      Element : Wide_Character;
      Set : Character_Set)
      return Boolean is
   begin
      return Overloaded_Is_In (
         Characters.Inside.To_Wide_Wide_Character (Element),
         Set);
   end Overloaded_Is_In;

   function Overloaded_Is_In (
      Element : Wide_Wide_Character;
      Set : Character_Set)
      return Boolean is
   begin
      pragma Assert (Valid (Set.Data));
      return Characters.Inside.Sets.Is_In (Element, Set.Data);
   end Overloaded_Is_In;

   function Overloaded_To_Ranges (Set : Character_Set)
      return Character_Ranges is
   begin
      pragma Assert (Valid (Set.Data));
      return Result : Character_Ranges (Set.Data.Items'Range) do
         for I in Result'Range loop
            Result (I).Low := Characters.Inside.To_Character (
               Set.Data.Items (I).Low);
            Result (I).High := Characters.Inside.To_Character (
               Set.Data.Items (I).High);
         end loop;
      end return;
   end Overloaded_To_Ranges;

   function Overloaded_To_Ranges (Set : Character_Set)
      return Wide_Character_Ranges is
   begin
      pragma Assert (Valid (Set.Data));
      return Result : Wide_Character_Ranges (Set.Data.Items'Range) do
         for I in Result'Range loop
            Result (I).Low := Characters.Inside.To_Wide_Character (
               Set.Data.Items (I).Low);
            Result (I).High := Characters.Inside.To_Wide_Character (
               Set.Data.Items (I).High);
         end loop;
      end return;
   end Overloaded_To_Ranges;

   function Overloaded_To_Ranges (Set : Character_Set)
      return Wide_Wide_Character_Ranges is
   begin
      pragma Assert (Valid (Set.Data));
      return Result : Wide_Wide_Character_Ranges (
         Set.Data.Items'Range)
      do
         for I in Result'Range loop
            Result (I).Low := Set.Data.Items (I).Low;
            Result (I).High := Set.Data.Items (I).High;
         end loop;
      end return;
   end Overloaded_To_Ranges;

   function Overloaded_To_Sequence (Set : Character_Set)
      return Character_Sequence is
   begin
      return System.UTF_Conversions.From_32_To_8.Convert (
         Overloaded_To_Sequence (Set));
   end Overloaded_To_Sequence;

   function Overloaded_To_Sequence (Set : Character_Set)
      return Wide_Character_Sequence is
   begin
      return System.UTF_Conversions.From_32_To_16.Convert (
         Overloaded_To_Sequence (Set));
   end Overloaded_To_Sequence;

   function Overloaded_To_Sequence (Set : Character_Set)
      return Wide_Wide_Character_Sequence
   is
      pragma Assert (Valid (Set.Data));
      Length : Natural := 0;
      Position : Positive;
   begin
      for I in Set.Data.Items'Range loop
         Length := Length + (
            Wide_Wide_Character'Pos (Set.Data.Items (I).High)
            - Wide_Wide_Character'Pos (Set.Data.Items (I).Low) + 1);
      end loop;
      return Result : Wide_Wide_String (1 .. Length) do
         Position := 1;
         for I in Set.Data.Items'Range loop
            for J in Set.Data.Items (I).Low .. Set.Data.Items (I).High loop
               Result (Position) := J;
               Position := Position + 1;
            end loop;
         end loop;
      end return;
   end Overloaded_To_Sequence;

   function Overloaded_To_Set (Ranges : Character_Ranges)
      return Character_Set
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
               Characters.Inside.To_Wide_Wide_Character (Ranges (I).Low),
               Characters.Inside.To_Wide_Wide_Character (Ranges (I).High));
         end if;
      end loop;
      if Last < Items'First then
         Data := Empty_Set_Data'Unrestricted_Access;
      else
         Data := new Set_Data'(
            Length => Last,
            Reference_Count => 1,
            Items => Data.Items (1 .. Last));
         pragma Assert (Valid (Data));
      end if;
      return (Finalization.Controlled with Data => Data);
   end Overloaded_To_Set;

   function Overloaded_To_Set (Ranges : Wide_Character_Ranges)
      return Character_Set
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
               Characters.Inside.To_Wide_Wide_Character (Ranges (I).Low),
               Characters.Inside.To_Wide_Wide_Character (Ranges (I).High));
         end if;
      end loop;
      if Last < Items'First then
         Data := Empty_Set_Data'Unrestricted_Access;
      else
         Data := new Set_Data'(
            Length => Last,
            Reference_Count => 1,
            Items => Data.Items (1 .. Last));
         pragma Assert (Valid (Data));
      end if;
      return (Finalization.Controlled with Data => Data);
   end Overloaded_To_Set;

   function Overloaded_To_Set (Ranges : Wide_Wide_Character_Ranges)
      return Character_Set
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
         Data := Empty_Set_Data'Unrestricted_Access;
      else
         Data := new Set_Data'(
            Length => Last,
            Reference_Count => 1,
            Items => Data.Items (1 .. Last));
         pragma Assert (Valid (Data));
      end if;
      return (Finalization.Controlled with Data => Data);
   end Overloaded_To_Set;

   function Overloaded_To_Set (Span : Character_Range)
      return Character_Set is
   begin
      return Overloaded_To_Set (Wide_Wide_Character_Range'(
         Low => Characters.Inside.To_Wide_Wide_Character (Span.Low),
         High => Characters.Inside.To_Wide_Wide_Character (Span.High)));
   end Overloaded_To_Set;

   function Overloaded_To_Set (Span : Wide_Character_Range)
      return Character_Set is
   begin
      return Overloaded_To_Set (Wide_Wide_Character_Range'(
         Low => Characters.Inside.To_Wide_Wide_Character (Span.Low),
         High => Characters.Inside.To_Wide_Wide_Character (Span.High)));
   end Overloaded_To_Set;

   function Overloaded_To_Set (Span : Wide_Wide_Character_Range)
      return Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Span.Low > Span.High then
         Data := Empty_Set_Data'Unrestricted_Access;
      else
         Data := new Set_Data'(
            Length => 1,
            Reference_Count => 1,
            Items => <>);
         Data.Items (Data.Items'First).Low := Span.Low;
         Data.Items (Data.Items'First).High := Span.High;
      end if;
      return (Finalization.Controlled with Data => Data);
   end Overloaded_To_Set;

   function Overloaded_To_Set (Sequence : Character_Sequence)
      return Character_Set is
   begin
      return Overloaded_To_Set (
         System.UTF_Conversions.From_8_To_32.Convert (Sequence));
   end Overloaded_To_Set;

   function Overloaded_To_Set (Sequence : Wide_Character_Sequence)
      return Character_Set is
   begin
      return Overloaded_To_Set (
         System.UTF_Conversions.From_16_To_32.Convert (Sequence));
   end Overloaded_To_Set;

   function Overloaded_To_Set (Sequence : Wide_Wide_Character_Sequence)
      return Character_Set
   is
      Items : Characters.Inside.Sets.Character_Ranges (Sequence'Range);
      Last : Natural := Items'First - 1;
      Data : Set_Data_Access;
   begin
      --  it should be more optimized...
      for I in Sequence'Range loop
         Characters.Inside.Sets.Add (
            Items,
            Last,
            Sequence (I),
            Sequence (I));
      end loop;
      if Last < Items'First then
         Data := Empty_Set_Data'Unrestricted_Access;
      else
         Data := new Set_Data'(
            Length => Last - Items'First + 1,
            Reference_Count => 1,
            Items => Items (Items'First .. Last));
         pragma Assert (Valid (Data));
      end if;
      return (Finalization.Controlled with Data => Data);
   end Overloaded_To_Set;

   function Overloaded_To_Set (Singleton : Character)
      return Character_Set is
   begin
      return Overloaded_To_Set (
         Characters.Inside.To_Wide_Wide_Character (Singleton));
   end Overloaded_To_Set;

   function Overloaded_To_Set (Singleton : Wide_Character)
      return Character_Set is
   begin
      return Overloaded_To_Set (
         Characters.Inside.To_Wide_Wide_Character (Singleton));
   end Overloaded_To_Set;

   function Overloaded_To_Set (Singleton : Wide_Wide_Character)
      return Character_Set is
   begin
      return (Finalization.Controlled with
         Data => new Set_Data'(
            Length => 1,
            Reference_Count => 1,
            Items => (1 => (Low => Singleton, High => Singleton))));
   end Overloaded_To_Set;

   function "=" (Left, Right : Character_Set) return Boolean is
   begin
      pragma Assert (Valid (Left.Data));
      pragma Assert (Valid (Right.Data));
      return Left.Data = Right.Data or else Left.Data.Items = Right.Data.Items;
   end "=";

   function "not" (Right : Character_Set) return Character_Set is
      Data : Set_Data_Access;
   begin
      if Right.Data.Length = 0 then
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
               Data := Empty_Set_Data'Unrestricted_Access;
            else
               Data := new Set_Data'(
                  Length => Last - Items'First + 1,
                  Reference_Count => 1,
                  Items => Items (Items'First .. Last));
               pragma Assert (Valid (Data));
            end if;
         end;
      end if;
      return (Finalization.Controlled with Data => Data);
   end "not";

   function "and" (Left, Right : Character_Set)
      return Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Left.Data.Length = 0 or else Right.Data.Length = 0 then
         Data := Empty_Set_Data'Unrestricted_Access;
      else
         declare
            Items : Characters.Inside.Sets.Character_Ranges (
               1 ..
               Left.Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Mul (Items, Last, Left.Data.Items, Right.Data.Items);
            if Last < Items'First then
               Data := Empty_Set_Data'Unrestricted_Access;
            else
               Data := new Set_Data'(
                  Length => Last - Items'First + 1,
                  Reference_Count => 1,
                  Items => Items (Items'First .. Last));
               pragma Assert (Valid (Data));
            end if;
         end;
      end if;
      return (Finalization.Controlled with Data => Data);
   end "and";

   function "or" (Left, Right : Character_Set)
      return Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Left.Data.Length = 0 then
         Data := Right.Data;
         System.Reference_Counting.Adjust (Data.Reference_Count'Access);
      elsif Right.Data.Length = 0 then
         Data := Left.Data;
         System.Reference_Counting.Adjust (Data.Reference_Count'Access);
      else
         declare
            Items : Characters.Inside.Sets.Character_Ranges (
               1 ..
               Left.Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Characters.Inside.Sets.Merge (
               Items,
               Last,
               Left.Data.Items,
               Right.Data.Items);
            Data := new Set_Data'(
               Length => Last - Items'First + 1,
               Reference_Count => 1,
               Items => Items (Items'First .. Last));
            pragma Assert (Valid (Data));
         end;
      end if;
      return (Finalization.Controlled with Data => Data);
   end "or";

   function "xor" (Left, Right : Character_Set)
      return Character_Set is
      Data : Set_Data_Access;
   begin
      if Left.Data.Length = 0 then
         Data := Right.Data;
         System.Reference_Counting.Adjust (Data.Reference_Count'Access);
      elsif Right.Data.Length = 0 then
         Data := Left.Data;
         System.Reference_Counting.Adjust (Data.Reference_Count'Access);
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
            Characters.Inside.Sets.Merge (
               X,
               X_Last,
               Left.Data.Items,
               Right.Data.Items);
            Mul (Y, Y_Last, Left.Data.Items, Right.Data.Items);
            Sub (Items, Last, X (1 .. X_Last), Y (1 .. Y_Last));
            if Last < Items'First then
               Data := Empty_Set_Data'Unrestricted_Access;
            else
               Data := new Set_Data'(
                  Length => Last - Items'First + 1,
                  Reference_Count => 1,
                  Items => Items (Items'First .. Last));
               pragma Assert (Valid (Data));
            end if;
         end;
      end if;
      return (Finalization.Controlled with Data => Data);
   end "xor";

   function "-" (Left, Right : Character_Set)
      return Character_Set
   is
      Data : Set_Data_Access;
   begin
      if Left.Data.Length = 0 then
         Data := Empty_Set_Data'Unrestricted_Access;
      elsif Right.Data.Length = 0 then
         Data := Left.Data;
         System.Reference_Counting.Adjust (Data.Reference_Count'Access);
      else
         declare
            Items : Characters.Inside.Sets.Character_Ranges (
               1 ..
               Left.Data.Length + Right.Data.Length);
            Last : Natural;
         begin
            Sub (Items, Last, Left.Data.Items, Right.Data.Items);
            if Last < Items'First then
               Data := Empty_Set_Data'Unrestricted_Access;
            else
               Data := new Set_Data'(
                  Length => Last - Items'First + 1,
                  Reference_Count => 1,
                  Items => Items (Items'First .. Last));
               pragma Assert (Valid (Data));
            end if;
         end;
      end if;
      return (Finalization.Controlled with Data => Data);
   end "-";

   package body No_Primitives_For_Set is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Character_Set)
      is
         Length : Integer;
         New_Data : Set_Data_Access;
      begin
         Integer'Read (Stream, Length);
         if Length = 0 then
            New_Data := Empty_Set_Data'Unrestricted_Access;
         else
            New_Data := new Set_Data'(
               Length => Length,
               Reference_Count => 1,
               Items => <>);
            begin
               Characters.Inside.Sets.Character_Ranges'Read (
                  Stream,
                  New_Data.Items);
               pragma Assert (Valid (Item.Data));
            exception
               when others =>
                  Free (New_Data);
                  raise;
            end;
         end if;
         Finalize (Item);
         Item.Data := New_Data;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Character_Set) is
      begin
         Integer'Write (Stream, Item.Data.Length);
         Characters.Inside.Sets.Character_Ranges'Write (
            Stream,
            Item.Data.Items);
      end Write;

   end No_Primitives_For_Set;

   --  maps

   procedure Free is new Unchecked_Deallocation (Map_Data, Map_Data_Access);

   procedure Free_Map_Data (Data : System.Address);
   procedure Free_Map_Data (Data : System.Address) is
      package Conv is new System.Address_To_Named_Access_Conversions (
         Map_Data,
         Map_Data_Access);
      X : Map_Data_Access := Conv.To_Pointer (Data);
   begin
      Free (X);
   end Free_Map_Data;

   overriding procedure Adjust (Object : in out Character_Mapping) is
   begin
      System.Reference_Counting.Adjust (Object.Data.Reference_Count'Access);
   end Adjust;

   overriding procedure Finalize (Object : in out Character_Mapping) is
      subtype Not_Null_Map_Data_Access is not null Map_Data_Access;
      type Map_Data_Access_Access is access all Not_Null_Map_Data_Access;
      type System_Address_Access is access all System.Address;
      function Upcast is new Unchecked_Conversion (
         Map_Data_Access_Access,
         System_Address_Access);
   begin
      System.Reference_Counting.Clear (
         Upcast (Object.Data'Access),
         Object.Data.Reference_Count'Access,
         Free => Free_Map_Data'Access);
   end Finalize;

   function Identity return Character_Mapping is
   begin
      return (Finalization.Controlled with
         Data => Empty_Map_Data'Unrestricted_Access);
   end Identity;

   function Is_Identity (Map : Character_Mapping) return Boolean is
   begin
      return Map.Data.Length = 0;
   end Is_Identity;

   function Overloaded_To_Domain (Map : Character_Mapping)
      return Character_Sequence is
   begin
      return System.UTF_Conversions.From_32_To_8.Convert (
         Overloaded_To_Domain (Map));
   end Overloaded_To_Domain;

   function Overloaded_To_Domain (Map : Character_Mapping)
      return Wide_Character_Sequence is
   begin
      return System.UTF_Conversions.From_32_To_16.Convert (
         Overloaded_To_Domain (Map));
   end Overloaded_To_Domain;

   function Overloaded_To_Domain (Map : Character_Mapping)
      return Wide_Wide_Character_Sequence is
   begin
      return Map.Data.From;
   end Overloaded_To_Domain;

   function Overloaded_To_Mapping (From, To : Character_Sequence)
      return Character_Mapping is
   begin
      return Overloaded_To_Mapping (
         System.UTF_Conversions.From_8_To_32.Convert (From),
         System.UTF_Conversions.From_8_To_32.Convert (To));
   end Overloaded_To_Mapping;

   function Overloaded_To_Mapping (From, To : Wide_Character_Sequence)
      return Character_Mapping is
   begin
      return Overloaded_To_Mapping (
         System.UTF_Conversions.From_16_To_32.Convert (From),
         System.UTF_Conversions.From_16_To_32.Convert (To));
   end Overloaded_To_Mapping;

   function Overloaded_To_Mapping (From, To : Wide_Wide_Character_Sequence)
      return Character_Mapping
   is
      New_Data : Map_Data_Access;
   begin
      if From'Length = 0 then
         New_Data := Empty_Map_Data'Unrestricted_Access;
      else
         New_Data := new Map_Data'(Characters.Inside.Maps.To_Mapping (
            From,
            To,
            Initial_Reference_Count => 1));
      end if;
      return (Finalization.Controlled with Data => New_Data);
   end Overloaded_To_Mapping;

   function Overloaded_To_Range (Map : Character_Mapping)
      return Character_Sequence is
   begin
      return System.UTF_Conversions.From_32_To_8.Convert (
         Overloaded_To_Range (Map));
   end Overloaded_To_Range;

   function Overloaded_To_Range (Map : Character_Mapping)
      return Wide_Character_Sequence is
   begin
      return System.UTF_Conversions.From_32_To_16.Convert (
         Overloaded_To_Range (Map));
   end Overloaded_To_Range;

   function Overloaded_To_Range (Map : Character_Mapping)
      return Wide_Wide_Character_Sequence is
   begin
      return Map.Data.To;
   end Overloaded_To_Range;

   function Overloaded_Value (
      Map : Character_Mapping;
      Element : Character)
      return Character is
   begin
      return Characters.Inside.To_Character (Overloaded_Value (
         Map,
         Characters.Inside.To_Wide_Wide_Character (Element)));
   end Overloaded_Value;

   function Overloaded_Value (
      Map : Character_Mapping;
      Element : Wide_Character)
      return Wide_Character is
   begin
      return Characters.Inside.To_Wide_Character (Overloaded_Value (
         Map,
         Characters.Inside.To_Wide_Wide_Character (Element)));
   end Overloaded_Value;

   function Overloaded_Value (
      Map : Character_Mapping;
      Element : Wide_Wide_Character)
      return Wide_Wide_Character is
   begin
      return Characters.Inside.Maps.Value (Map.Data, Element);
   end Overloaded_Value;

   function "=" (Left, Right : Character_Mapping) return Boolean is
   begin
      return Left.Data = Right.Data
         or else (
            Left.Data.From = Right.Data.From
            and then Left.Data.To = Right.Data.To);
   end "=";

   package body No_Primitives_For_Map is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Character_Mapping)
      is
         Length : Integer;
         New_Data : Map_Data_Access;
      begin
         Integer'Read (Stream, Length);
         if Length = 0 then
            New_Data := Empty_Map_Data'Unrestricted_Access;
         else
            New_Data := new Map_Data'(
               Length => Length,
               Reference_Count => 1,
               From => <>,
               To => <>);
            begin
               System.Strings.Stream_Ops.Wide_Wide_String_Read_Blk_IO (
                  Stream,
                  New_Data.From);
               System.Strings.Stream_Ops.Wide_Wide_String_Read_Blk_IO (
                  Stream,
                  New_Data.To);
            exception
               when others =>
                  Free (New_Data);
                  raise;
            end;
         end if;
         Finalize (Item);
         Item.Data := New_Data;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Character_Mapping) is
      begin
         Integer'Write (Stream, Item.Data.Length);
         System.Strings.Stream_Ops.Wide_Wide_String_Write_Blk_IO (
            Stream,
            Item.Data.From);
         System.Strings.Stream_Ops.Wide_Wide_String_Write_Blk_IO (
            Stream,
            Item.Data.To);
      end Write;

   end No_Primitives_For_Map;

end Ada.Strings.Maps;
