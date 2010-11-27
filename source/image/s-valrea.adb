with System.Val_Uns;
with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising);
package body System.Val_Real is
   pragma Suppress (All_Checks);
   use type Formatting.Unsigned;

   procedure Get_Decimal (
      S : String;
      Index : in out Positive;
      Result : out Long_Long_Float;
      Base : Formatting.Base_Type);
   procedure Get_Decimal (
      S : String;
      Index : in out Positive;
      Result : out Long_Long_Float;
      Base : Formatting.Base_Type)
   is
      First : constant Positive := Index;
      Scale : Long_Long_Float := 1.0;
   begin
      Result := 0.0;
      Index := Index + 1; --  '.'
      while Index <= S'Last loop
         declare
            X : Formatting.Digit;
            Is_Invalid : Boolean;
         begin
            if S (Index) = '_' then
               exit when Index = First or else Index >= S'Last;
               Index := Index + 1;
            end if;
            Formatting.Value (S (Index), X, Is_Invalid);
            exit when Is_Invalid or else X >= Base;
            if Scale <= Long_Long_Float'Last / Long_Long_Float (Base) then
               Result := Result * Long_Long_Float (Base) + Long_Long_Float (X);
               Scale := Scale * Long_Long_Float (Base);
            end if;
            Index := Index + 1;
         end;
      end loop;
      Result := Result / Scale;
   end Get_Decimal;

   procedure Get_Unsigned_Real (
      S : String;
      Index : in out Positive;
      Result : out Long_Long_Float;
      Base : Formatting.Base_Type)
   is
      First : constant Positive := Index;
   begin
      Result := 0.0;
      while Index <= S'Last loop
         declare
            X : Formatting.Digit;
            Is_Invalid : Boolean;
         begin
            if S (Index) = '_' then
               exit when Index = First or else Index >= S'Last;
               Index := Index + 1;
            elsif S (Index) = '.' then
               declare
                  Decimal : Long_Long_Float;
               begin
                  Get_Decimal (S, Index, Decimal, Base);
                  Result := Result + Decimal;
               end;
               exit;
            end if;
            Formatting.Value (S (Index), X, Is_Invalid);
            exit when Is_Invalid or else X >= Base;
            if Result > (Long_Long_Float'Last - Long_Long_Float (X)) /
               Long_Long_Float (Base)
            then
               Unwind.Raising.Overflow;
            end if;
            Result := Result * Long_Long_Float (Base) + Long_Long_Float (X);
            Index := Index + 1;
         end;
      end loop;
   end Get_Unsigned_Real;

   function Value_Real (Str : String) return Long_Long_Float is
      Index : Positive := Str'First;
      Result : Long_Long_Float;
      Sign : Long_Long_Float;
      Base : Formatting.Base_Type := 10;
      Exponent : Integer;
   begin
      Val_Uns.Skip_Spaces (Str, Index);
      if Index <= Str'Last and then Str (Index) = '-' then
         Index := Index + 1;
         Sign := -1.0;
      else
         if Index <= Str'Last and then Str (Index) = '+' then
            Index := Index + 1;
         end if;
         Sign := 1.0;
      end if;
      Get_Unsigned_Real (Str, Index, Result, Base => Base);
      if Index <= Str'Last and then Str (Index) = '#' then
         Index := Index + 1;
         if Result /= Long_Long_Float'Truncation (Result)
            or else Result < Long_Long_Float (Formatting.Base_Type'First)
            or else Result > Long_Long_Float (Formatting.Base_Type'Last)
         then
            raise Constraint_Error;
         end if;
         Base := Formatting.Base_Type (Result);
         Get_Unsigned_Real (Str, Index, Result, Base => Base);
         if Index > Str'Last or else Str (Index) /= '#' then
            raise Constraint_Error;
         end if;
         Index := Index + 1;
      end if;
      Val_Uns.Get_Exponent (Str, Index, Exponent, Positive_Only => False);
      if Exponent /= 0 then
         Result := Result * Long_Long_Float (Base) ** Exponent;
      end if;
      Val_Uns.Check_Last (Str, Index);
      return Long_Long_Float'Copy_Sign (Result, Sign);
   end Value_Real;

end System.Val_Real;
