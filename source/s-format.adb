package body System.Formatting is
   pragma Suppress (All_Checks);

   function Width_Digits (Value : Unsigned; Base : Number_Base)
      return Positive;
   function Width_Digits (Value : Unsigned; Base : Number_Base)
      return Positive
   is
      V : Unsigned := Value;
      Result : Positive := 1;
   begin
      while V >= Unsigned (Base) loop
         V := V / Unsigned (Base);
         Result := Result + 1;
      end loop;
      return Result;
   end Width_Digits;

   function Width_Digits (Value : Longest_Unsigned; Base : Number_Base)
      return Positive;
   function Width_Digits (Value : Longest_Unsigned; Base : Number_Base)
      return Positive is
   begin
      if Standard'Word_Size < Longest_Unsigned'Size then
         --  optimized for 32bit
         declare
            V : Longest_Unsigned := Value;
            Offset : Natural := 0;
         begin
            while V > Longest_Unsigned (Unsigned'Last) loop
               V := V / Longest_Unsigned (Base);
               Offset := Offset + 1;
            end loop;
            return Offset + Width_Digits (Unsigned (V), Base);
         end;
      else
         --  optimized for 64bit
         declare
            V : Longest_Unsigned := Value;
            Result : Positive := 1;
         begin
            while V >= Longest_Unsigned (Base) loop
               V := V / Longest_Unsigned (Base);
               Result := Result + 1;
            end loop;
            return Result;
         end;
      end if;
   end Width_Digits;

   procedure Fill_Padding (Item : out String; Padding : Character);
   procedure Fill_Padding (Item : out String; Padding : Character) is
   begin
      for I in Item'Range loop
         Item (I) := Padding;
      end loop;
   end Fill_Padding;

   procedure Fill_Digits (
      Value : Unsigned;
      Item : out String;
      Base : Number_Base;
      Set : Type_Set);
   procedure Fill_Digits (
      Value : Unsigned;
      Item : out String;
      Base : Number_Base;
      Set : Type_Set)
   is
      V : Unsigned := Value;
   begin
      for I in reverse Item'Range loop
         Image (Digit (V rem Unsigned (Base)), Item (I), Set);
         V := V / Unsigned (Base);
      end loop;
   end Fill_Digits;

   procedure Fill_Digits (
      Value : Longest_Unsigned;
      Item : out String;
      Base : Number_Base;
      Set : Type_Set);
   procedure Fill_Digits (
      Value : Longest_Unsigned;
      Item : out String;
      Base : Number_Base;
      Set : Type_Set) is
   begin
      if Standard'Word_Size < Longest_Unsigned'Size then
         --  optimized for 32bit
         declare
            V : Longest_Unsigned := Value;
            I : Positive := Item'Last;
         begin
            while V > Longest_Unsigned (Unsigned'Last) loop
               Image (Digit (V rem Longest_Unsigned (Base)), Item (I), Set);
               V := V / Longest_Unsigned (Base);
               I := I - 1;
            end loop;
            Fill_Digits (Unsigned (V), Item (Item'First .. I), Base, Set);
         end;
      else
         --  optimized for 64bit
         declare
            V : Longest_Unsigned := Value;
         begin
            for I in reverse Item'Range loop
               Image (Digit (V rem Longest_Unsigned (Base)), Item (I), Set);
               V := V / Longest_Unsigned (Base);
            end loop;
         end;
      end if;
   end Fill_Digits;

   procedure Take_Digits (
      Item : String;
      Last : out Natural;
      Result : out Unsigned;
      Base : Number_Base;
      Skip_Underscore : Boolean;
      Overflow : out Boolean);
   procedure Take_Digits (
      Item : String;
      Last : out Natural;
      Result : out Unsigned;
      Base : Number_Base;
      Skip_Underscore : Boolean;
      Overflow : out Boolean) is
   begin
      Last := Item'First - 1;
      Result := 0;
      Overflow := False;
      while Last < Item'Last loop
         declare
            X : Digit;
            Is_Invalid : Boolean;
            Next : Positive := Last + 1;
         begin
            if Item (Next) = '_' then
               exit when not Skip_Underscore
                  or else Next = Item'First
                  or else Next >= Item'Last;
               Next := Next + 1;
            end if;
            Value (Item (Next), X, Is_Invalid);
            exit when Is_Invalid or else X >= Base;
            if Result > (Unsigned'Last - Unsigned (X)) / Unsigned (Base) then
               Overflow := True;
               exit;
            end if;
            Result := Result * Unsigned (Base) + Unsigned (X);
            Last := Next;
         end;
      end loop;
   end Take_Digits;

   procedure Take_Digits (
      Item : String;
      Last : out Natural;
      Result : out Longest_Unsigned;
      Base : Number_Base;
      Skip_Underscore : Boolean;
      Overflow : out Boolean);
   procedure Take_Digits (
      Item : String;
      Last : out Natural;
      Result : out Longest_Unsigned;
      Base : Number_Base;
      Skip_Underscore : Boolean;
      Overflow : out Boolean) is
   begin
      if Standard'Word_Size < Longest_Unsigned'Size then
         --  optimized for 32bit
         Take_Digits (
            Item,
            Last,
            Unsigned (Result),
            Base,
            Skip_Underscore,
            Overflow);
         if Overflow then
            Overflow := False;
            while Last < Item'Last loop
               declare
                  X : Digit;
                  Is_Invalid : Boolean;
                  Next : Positive := Last + 1;
               begin
                  if Item (Next) = '_' then
                     exit when not Skip_Underscore or else Next >= Item'Last;
                     Next := Next + 1;
                  end if;
                  Value (Item (Next), X, Is_Invalid);
                  exit when Is_Invalid or else X >= Base;
                  if Result >
                     (Longest_Unsigned'Last - Longest_Unsigned (X))
                     / Longest_Unsigned (Base)
                  then
                     Overflow := True;
                     exit;
                  end if;
                  Result := Result * Longest_Unsigned (Base)
                     + Longest_Unsigned (X);
                  Last := Next;
               end;
            end loop;
         end if;
      else
         --  optimized for 64bit
         Last := Item'First - 1;
         Result := 0;
         Overflow := False;
         while Last < Item'Last loop
            declare
               X : Digit;
               Is_Invalid : Boolean;
               Next : Positive := Last + 1;
            begin
               if Item (Next) = '_' then
                  exit when not Skip_Underscore
                     or else Next = Item'First
                     or else Next >= Item'Last;
                  Next := Next + 1;
               end if;
               Value (Item (Next), X, Is_Invalid);
               exit when Is_Invalid or else X >= Base;
               if Result >
                  (Longest_Unsigned'Last - Longest_Unsigned (X))
                  / Longest_Unsigned (Base)
               then
                  Overflow := True;
                  exit;
               end if;
               Result := Result * Longest_Unsigned (Base)
                  + Longest_Unsigned (X);
               Last := Next;
            end;
         end loop;
      end if;
   end Take_Digits;

   --  implementation

   function Width (Value : Unsigned; Base : Number_Base := 10)
      return Positive is
   begin
      if Standard'Word_Size < Longest_Unsigned'Size then
         --  optimized for 32bit
         return Width_Digits (Value, Base);
      else
         --  optimized for 64bit
         return Width (Longest_Unsigned (Value), Base);
      end if;
   end Width;

   function Width (Value : Longest_Unsigned; Base : Number_Base := 10)
      return Positive is
   begin
      return Width_Digits (Value, Base);
   end Width;

   procedure Image (
      Value : Digit;
      Item : out Character;
      Set : Type_Set := Upper_Case) is
   begin
      case Value is
         when 0 .. 9 =>
            Item := Character'Val (Character'Pos ('0') + Value);
         when 10 .. 15 =>
            Item := Character'Val (
               Character'Pos ('a')
               - 10
               - (Character'Pos ('a') - Character'Pos ('A'))
                  * Type_Set'Pos (Set)
               + Value);
      end case;
   end Image;

   procedure Image (
      Value : Unsigned;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Set : Type_Set := Upper_Case;
      Width : Positive := 1;
      Padding : Character := '0';
      Error : out Boolean) is
   begin
      if Standard'Word_Size < Longest_Unsigned'Size then
         --  optimized for 32bit
         declare
            W : constant Positive := Formatting.Width (Value, Base);
            Padding_Length : constant Natural := Integer'Max (0, Width - W);
            Length : constant Natural := Padding_Length + W;
         begin
            Error := Length > Item'Length;
            if Error then
               Last := Item'First - 1;
            else
               Last := Item'First + Length - 1;
               Fill_Padding (
                  Item (Item'First .. Item'First + Padding_Length - 1),
                  Padding);
               Fill_Digits (
                  Value,
                  Item (Item'First + Padding_Length .. Last),
                  Base,
                  Set);
            end if;
         end;
      else
         --  optimized for 64bit
         Image (
            Longest_Unsigned (Value),
            Item,
            Last,
            Base,
            Set,
            Width,
            Padding,
            Error);
      end if;
   end Image;

   procedure Image (
      Value : Longest_Unsigned;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Set : Type_Set := Upper_Case;
      Width : Positive := 1;
      Padding : Character := '0';
      Error : out Boolean)
   is
      W : constant Positive := Formatting.Width (Value, Base);
      Padding_Length : constant Natural := Integer'Max (0, Width - W);
      Length : constant Natural := Padding_Length + W;
   begin
      Error := Length > Item'Length;
      if Error then
         Last := Item'First - 1;
      else
         Last := Item'First + Length - 1;
         Fill_Padding (
            Item (Item'First .. Item'First + Padding_Length - 1),
            Padding);
         Fill_Digits (
            Value,
            Item (Item'First + Padding_Length .. Last),
            Base,
            Set);
      end if;
   end Image;

   procedure Value (
      Item : Character;
      Result : out Digit;
      Error : out Boolean) is
   begin
      case Item is
         when '0' .. '9' =>
            Result := Character'Pos (Item) - Character'Pos ('0');
            Error := False;
         when 'A' .. 'F' =>
            Result := Character'Pos (Item) - (Character'Pos ('A') - 10);
            Error := False;
         when 'a' .. 'f' =>
            Result := Character'Pos (Item) - (Character'Pos ('a') - 10);
            Error := False;
         when others =>
            Error := True;
      end case;
   end Value;

   procedure Value (
      Item : String;
      Last : out Natural;
      Result : out Unsigned;
      Base : Number_Base := 10;
      Skip_Underscore : Boolean := False;
      Error : out Boolean) is
   begin
      if Standard'Word_Size < Longest_Unsigned'Size then
         --  optimized for 32bit
         declare
            Overflow : Boolean;
         begin
            Take_Digits (Item, Last, Result, Base, Skip_Underscore, Overflow);
            if Overflow then
               Result := 0;
               Last := Item'First - 1;
               Error := True;
            else
               Error := Last < Item'First;
            end if;
         end;
      else
         --  optimized for 64bit
         declare
            Longest_Result : Longest_Unsigned;
         begin
            Value (Item, Last, Longest_Result, Base, Skip_Underscore, Error);
            if Longest_Result > Longest_Unsigned (Unsigned'Last) then
               Result := 0;
               Last := Item'First - 1;
               Error := True;
            else
               Result := Unsigned (Longest_Result);
            end if;
         end;
      end if;
   end Value;

   procedure Value (
      Item : String;
      Last : out Natural;
      Result : out Longest_Unsigned;
      Base : Number_Base := 10;
      Skip_Underscore : Boolean := False;
      Error : out Boolean)
   is
      Overflow : Boolean;
   begin
      Take_Digits (Item, Last, Result, Base, Skip_Underscore, Overflow);
      if Overflow then
         Result := 0;
         Last := Item'First - 1;
         Error := True;
      else
         Error := Last < Item'First;
      end if;
   end Value;

end System.Formatting;
