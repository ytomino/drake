with System.Formatting;
with System.Val_Uns;
with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising); -- break "pure" rule
package body System.Val_Real is
   pragma Suppress (All_Checks);
   use type Formatting.Unsigned;

   procedure Get_Aft (
      S : String;
      Last : in out Natural;
      Result : out Long_Long_Float;
      Base : Formatting.Number_Base);
   procedure Get_Aft (
      S : String;
      Last : in out Natural;
      Result : out Long_Long_Float;
      Base : Formatting.Number_Base)
   is
      Scale : Long_Long_Float := 1.0;
      Old_Last : constant Natural := Last + 1; -- skip '.'
   begin
      Last := Old_Last;
      Result := 0.0;
      while Last < S'Last loop
         declare
            X : Formatting.Digit;
            Is_Invalid : Boolean;
         begin
            if S (Last + 1) = '_' then
               exit when Last = Old_Last or else Last + 1 >= S'Last;
               Last := Last + 1;
            end if;
            Formatting.Value (S (Last + 1), X, Is_Invalid);
            exit when Is_Invalid or else X >= Formatting.Unsigned (Base);
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
      S : String;
      Last : in out Natural;
      Result : out Long_Long_Float;
      Base : Formatting.Number_Base);
   procedure Get_Unsigned_Real (
      S : String;
      Last : in out Natural;
      Result : out Long_Long_Float;
      Base : Formatting.Number_Base)
   is
      Old_Last : constant Natural := Last;
   begin
      Result := 0.0;
      while Last < S'Last loop
         declare
            X : Formatting.Digit;
            Is_Invalid : Boolean;
         begin
            if S (Last + 1) = '_' then
               exit when Last = Old_Last or else Last + 1 >= S'Last;
               Last := Last + 1;
            elsif S (Last + 1) = '.' then
               declare
                  Decimal : Long_Long_Float;
               begin
                  Get_Aft (S, Last, Decimal, Base);
                  Result := Result + Decimal;
               end;
               exit;
            end if;
            Formatting.Value (S (Last + 1), X, Is_Invalid);
            exit when Is_Invalid or else X >= Formatting.Unsigned (Base);
            if Result > (Long_Long_Float'Last - Long_Long_Float (X)) /
               Long_Long_Float (Base)
            then
               Unwind.Raising.Overflow;
            end if;
            Result := Result * Long_Long_Float (Base) + Long_Long_Float (X);
            Last := Last + 1;
         end;
      end loop;
   end Get_Unsigned_Real;

   function Value_Real (Str : String) return Long_Long_Float is
      Last : Natural := Str'First - 1;
      Result : Long_Long_Float;
   begin
      Get_Float_Literal (Str, Last, Result);
      Val_Uns.Check_Last (Str, Last);
      return Result;
   end Value_Real;

   procedure Get_Float_Literal (
      S : String;
      Last : out Natural;
      Result : out Long_Long_Float)
   is
      Sign : Long_Long_Float;
      Base : Formatting.Number_Base := 10;
      Mark : Character;
      Exponent : Integer;
   begin
      Last := S'First - 1;
      Val_Uns.Skip_Spaces (S, Last);
      if Last < S'Last and then S (Last + 1) = '-' then
         Last := Last + 1;
         Sign := -1.0;
      else
         if Last < S'Last and then S (Last + 1) = '+' then
            Last := Last + 1;
         end if;
         Sign := 1.0;
      end if;
      Get_Unsigned_Real (S, Last, Result, Base => Base);
      if Last < S'Last
         and then (S (Last + 1) = '#' or else S (Last + 1) = ':')
      then
         Mark := S (Last + 1);
         Last := Last + 1;
         if Result /= Long_Long_Float'Truncation (Result)
            or else Result < Long_Long_Float (Formatting.Number_Base'First)
            or else Result > Long_Long_Float (Formatting.Number_Base'Last)
         then
            raise Constraint_Error;
         end if;
         Base := Formatting.Number_Base (Result);
         Get_Unsigned_Real (S, Last, Result, Base => Base);
         if Last >= S'Last or else S (Last + 1) /= Mark then
            raise Constraint_Error;
         end if;
         Last := Last + 1;
      end if;
      Val_Uns.Get_Exponent (S, Last, Exponent, Positive_Only => False);
      if Exponent /= 0 then
         Result := Result * Long_Long_Float (Base) ** Exponent;
      end if;
      Result := Long_Long_Float'Copy_Sign (Result, Sign);
   end Get_Float_Literal;

end System.Val_Real;
