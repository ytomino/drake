package body System.Formatting.Literals.Float is
   pragma Suppress (All_Checks);

   function copysignl (X, Y : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, copysignl, "__builtin_copysignl");
   function truncl (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, truncl, "__builtin_truncl");

   procedure Get_Aft (
      Item : String;
      Last : in out Natural;
      Result : out Long_Long_Float;
      Base : Number_Base);
   procedure Get_Aft (
      Item : String;
      Last : in out Natural;
      Result : out Long_Long_Float;
      Base : Number_Base)
   is
      Scale : Long_Long_Float := 1.0;
      Old_Last : constant Natural := Last + 1; -- skip '.'
   begin
      Last := Old_Last;
      Result := 0.0;
      while Last < Item'Last loop
         declare
            X : Digit;
            Is_Invalid : Boolean;
         begin
            if Item (Last + 1) = '_' then
               exit when Last = Old_Last or else Last + 1 >= Item'Last;
               Last := Last + 1;
            end if;
            Value (Item (Last + 1), X, Is_Invalid);
            exit when Is_Invalid or else X >= Base;
            if Scale <= Long_Long_Float'Last / Long_Long_Float (Base) then
               Result := Result * Long_Long_Float (Base) + Long_Long_Float (X);
               Scale := Scale * Long_Long_Float (Base);
            end if;
            Last := Last + 1;
         end;
      end loop;
      Result := Result / Scale;
   end Get_Aft;

   procedure Get_Unsigned_Real (
      Item : String;
      Last : in out Natural;
      Result : out Long_Long_Float;
      Base : Number_Base;
      Error : out Boolean);
   procedure Get_Unsigned_Real (
      Item : String;
      Last : in out Natural;
      Result : out Long_Long_Float;
      Base : Number_Base;
      Error : out Boolean)
   is
      Old_Last : constant Natural := Last;
   begin
      Result := 0.0;
      while Last < Item'Last loop
         declare
            X : Digit;
            Is_Invalid : Boolean;
         begin
            if Item (Last + 1) = '_' then
               exit when Last = Old_Last or else Last + 1 >= Item'Last;
               Last := Last + 1;
            elsif Item (Last + 1) = '.' then
               declare
                  Decimal : Long_Long_Float;
               begin
                  Get_Aft (Item, Last, Decimal, Base);
                  Result := Result + Decimal;
               end;
               exit;
            end if;
            Value (Item (Last + 1), X, Is_Invalid);
            exit when Is_Invalid or else X >= Base;
            if Result >
               (Long_Long_Float'Last - Long_Long_Float (X))
               / Long_Long_Float (Base)
            then
               Error := True;
               return;
            end if;
            Result := Result * Long_Long_Float (Base) + Long_Long_Float (X);
            Last := Last + 1;
         end;
      end loop;
      Error := False;
   end Get_Unsigned_Real;

   --  implementation

   procedure Get_Literal (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Float;
      Error : out Boolean)
   is
      Sign : Long_Long_Float;
      Base : Number_Base := 10;
      Mark : Character;
      Exponent : Integer;
   begin
      Last := Item'First - 1;
      Skip_Spaces (Item, Last);
      if Last < Item'Last and then Item (Last + 1) = '-' then
         Last := Last + 1;
         Sign := -1.0;
      else
         if Last < Item'Last and then Item (Last + 1) = '+' then
            Last := Last + 1;
         end if;
         Sign := 1.0;
      end if;
      Get_Unsigned_Real (Item, Last, Result, Base => Base, Error => Error);
      if not Error then
         if Last < Item'Last
            and then (Item (Last + 1) = '#' or else Item (Last + 1) = ':')
         then
            Mark := Item (Last + 1);
            Last := Last + 1;
            if Result = truncl (Result)
               and then Result in
                  Long_Long_Float (Number_Base'First) ..
                  Long_Long_Float (Number_Base'Last)
            then
               Base := Number_Base (Result);
               Get_Unsigned_Real (Item, Last, Result,
                  Base => Base,
                  Error => Error);
               if not Error then
                  if Last < Item'Last and then Item (Last + 1) = Mark then
                     Last := Last + 1;
                  else
                     Error := True;
                     return;
                  end if;
               else
                  return;
               end if;
            else
               Error := True;
               return;
            end if;
         end if;
         Get_Exponent (Item, Last, Exponent,
            Positive_Only => False,
            Error => Error);
         if not Error then
            if Exponent /= 0 then
               Result := Result * Long_Long_Float (Base) ** Exponent;
            end if;
            Result := copysignl (Result, Sign);
         end if;
      end if;
   end Get_Literal;

end System.Formatting.Literals.Float;
