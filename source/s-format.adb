with System.Storage_Elements;
package body System.Formatting is
   pragma Suppress (All_Checks);
   use type Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   procedure memset (
      b : Address;
      c : Integer;
      n : Storage_Elements.Storage_Count)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memset";

   function add_overflow (
      a, b : Word_Unsigned;
      res : not null access Word_Unsigned)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name =>
            (if Standard'Word_Size = Integer'Size then
                  "__builtin_uadd_overflow"
               elsif Standard'Word_Size = Long_Integer'Size then
                  "__builtin_uaddl_overflow"
               else "__builtin_uaddll_overflow");

   function add_overflow (
      a, b : Long_Long_Unsigned;
      res : not null access Long_Long_Unsigned)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__builtin_uaddll_overflow";

   function mul_overflow (
      a, b : Word_Unsigned;
      res : not null access Word_Unsigned)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name =>
            (if Standard'Word_Size = Integer'Size then
                  "__builtin_umul_overflow"
               elsif Standard'Word_Size = Long_Integer'Size then
                  "__builtin_umull_overflow"
               else "__builtin_umulll_overflow");

   function mul_overflow (
      a, b : Long_Long_Unsigned;
      res : not null access Long_Long_Unsigned)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__builtin_umulll_overflow";

   function Width_Digits (Value : Word_Unsigned; Base : Number_Base)
      return Positive;
   function Width_Digits (Value : Word_Unsigned; Base : Number_Base)
      return Positive
   is
      P : aliased Word_Unsigned := Word_Unsigned (Base);
      Result : Positive := 1;
   begin
      while P <= Value loop
         Result := Result + 1;
         exit when mul_overflow (P, Word_Unsigned (Base), P'Access);
      end loop;
      return Result;
   end Width_Digits;

   function Width_Digits (Value : Long_Long_Unsigned; Base : Number_Base)
      return Positive;
   function Width_Digits (Value : Long_Long_Unsigned; Base : Number_Base)
      return Positive is
   begin
      if Standard'Word_Size < Long_Long_Unsigned'Size then
         --  optimized for 32bit
         declare
            P : aliased Long_Long_Unsigned := Long_Long_Unsigned (Base);
            Result : Positive := 1;
         begin
            while P <= Value loop
               Result := Result + 1;
               exit when mul_overflow (P, Long_Long_Unsigned (Base), P'Access);
            end loop;
            return Result;
         end;
      else
         --  optimized for 64bit
         return Width_Digits (Word_Unsigned (Value), Base);
      end if;
   end Width_Digits;

   procedure Fill_Digits (
      Value : Word_Unsigned;
      Item : out String;
      Base : Number_Base;
      Set : Type_Set);
   procedure Fill_Digits (
      Value : Word_Unsigned;
      Item : out String;
      Base : Number_Base;
      Set : Type_Set)
   is
      V : Word_Unsigned := Value;
   begin
      for I in reverse Item'Range loop
         Image (Digit (V rem Word_Unsigned (Base)), Item (I), Set);
         V := V / Word_Unsigned (Base);
      end loop;
   end Fill_Digits;

   procedure Fill_Digits (
      Value : Long_Long_Unsigned;
      Item : out String;
      Base : Number_Base;
      Set : Type_Set);
   procedure Fill_Digits (
      Value : Long_Long_Unsigned;
      Item : out String;
      Base : Number_Base;
      Set : Type_Set) is
   begin
      if Standard'Word_Size < Long_Long_Unsigned'Size then
         --  optimized for 32bit
         declare
            V : Long_Long_Unsigned := Value;
            I : Positive := Item'Last;
         begin
            while V > Long_Long_Unsigned (Word_Unsigned'Last) loop
               Image (Digit (V rem Long_Long_Unsigned (Base)), Item (I), Set);
               V := V / Long_Long_Unsigned (Base);
               I := I - 1;
            end loop;
            Fill_Digits (Word_Unsigned (V), Item (Item'First .. I), Base, Set);
         end;
      else
         --  optimized for 64bit
         Fill_Digits (Word_Unsigned (Value), Item, Base, Set);
      end if;
   end Fill_Digits;

   procedure Take_Digits (
      Item : String;
      Last : out Natural;
      Result : out Word_Unsigned;
      Base : Number_Base;
      Skip_Underscore : Boolean;
      Overflow : out Boolean);
   procedure Take_Digits (
      Item : String;
      Last : out Natural;
      Result : out Word_Unsigned;
      Base : Number_Base;
      Skip_Underscore : Boolean;
      Overflow : out Boolean)
   is
      R : aliased Word_Unsigned := 0;
   begin
      Last := Item'First - 1;
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
            if mul_overflow (R, Word_Unsigned (Base), R'Access)
               or else add_overflow (R, Word_Unsigned (X), R'Access)
            then
               Overflow := True;
               exit;
            end if;
            Last := Next;
         end;
      end loop;
      Result := R;
   end Take_Digits;

   procedure Take_Digits (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Unsigned;
      Base : Number_Base;
      Skip_Underscore : Boolean;
      Overflow : out Boolean);
   procedure Take_Digits (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Unsigned;
      Base : Number_Base;
      Skip_Underscore : Boolean;
      Overflow : out Boolean) is
   begin
      if Standard'Word_Size < Long_Long_Unsigned'Size then
         --  optimized for 32bit
         declare
            R : aliased Long_Long_Unsigned := 0;
         begin
            Take_Digits (
               Item,
               Last,
               Word_Unsigned (R),
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
                        exit when not Skip_Underscore
                           or else Next >= Item'Last;
                        Next := Next + 1;
                     end if;
                     Value (Item (Next), X, Is_Invalid);
                     exit when Is_Invalid or else X >= Base;
                     if mul_overflow (R, Long_Long_Unsigned (Base), R'Access)
                        or else add_overflow (
                           R,
                           Long_Long_Unsigned (X),
                           R'Access)
                     then
                        Overflow := True;
                        exit;
                     end if;
                     Last := Next;
                  end;
               end loop;
            end if;
            Result := R;
         end;
      else
         --  optimized for 64bit
         Take_Digits (
            Item,
            Last,
            Word_Unsigned (Result),
            Base,
            Skip_Underscore,
            Overflow);
      end if;
   end Take_Digits;

   --  implementation

   function Width (Value : Word_Unsigned; Base : Number_Base := 10)
      return Positive is
   begin
      return Width_Digits (Value, Base);
   end Width;

   function Width (
      Value : Long_Long_Integer_Types.Long_Long_Unsigned;
      Base : Number_Base := 10)
      return Positive is
   begin
      if Standard'Word_Size < Long_Long_Unsigned'Size then
         --  optimized for 32bit
         return Width_Digits (Value, Base);
      else
         --  optimized for 64bit
         return Width (Word_Unsigned (Value), Base);
      end if;
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
      Value : Word_Unsigned;
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
      Error := Item'First + Length - 1 > Item'Last;
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

   procedure Image (
      Value : Long_Long_Integer_Types.Long_Long_Unsigned;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Set : Type_Set := Upper_Case;
      Width : Positive := 1;
      Padding : Character := '0';
      Error : out Boolean) is
   begin
      if Standard'Word_Size < Long_Long_Unsigned'Size then
         --  optimized for 32bit
         declare
            W : constant Positive := Formatting.Width (Value, Base);
            Padding_Length : constant Natural := Integer'Max (0, Width - W);
            Length : constant Natural := Padding_Length + W;
         begin
            Error := Item'First + Length - 1 > Item'Last;
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
            Word_Unsigned (Value),
            Item,
            Last,
            Base,
            Set,
            Width,
            Padding,
            Error);
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
      Result : out Word_Unsigned;
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

   procedure Value (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Integer_Types.Long_Long_Unsigned;
      Base : Number_Base := 10;
      Skip_Underscore : Boolean := False;
      Error : out Boolean) is
   begin
      if Standard'Word_Size < Long_Long_Unsigned'Size then
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
         Value (
            Item,
            Last,
            Word_Unsigned (Result),
            Base,
            Skip_Underscore,
            Error);
      end if;
   end Value;

   procedure Fill_Padding (Item : out String; Pad : Character) is
   begin
      memset (Item'Address, Character'Pos (Pad), Item'Length);
   end Fill_Padding;

end System.Formatting;
