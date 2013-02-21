with System.Formatting;
with System.Val_Uns;
with System.Value_Error;
package body System.Val_Real is
   pragma Suppress (All_Checks);
   use type Formatting.Unsigned;

   function copysignl (X, Y : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, copysignl, "__builtin_copysignl");
   function truncl (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, truncl, "__builtin_truncl");

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
      Base : Formatting.Number_Base;
      Error : out Boolean);
   procedure Get_Unsigned_Real (
      S : String;
      Last : in out Natural;
      Result : out Long_Long_Float;
      Base : Formatting.Number_Base;
      Error : out Boolean)
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

   function Value_Real (Str : String) return Long_Long_Float is
      Last : Natural := Str'First - 1;
      Result : Long_Long_Float;
      Error : Boolean;
   begin
      Get_Float_Literal (Str, Last, Result, Error);
      if not Error then
         Val_Uns.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Error ("Float", Str);
   end Value_Real;

   procedure Get_Float_Literal (
      S : String;
      Last : out Natural;
      Result : out Long_Long_Float;
      Error : out Boolean)
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
      Get_Unsigned_Real (S, Last, Result, Base => Base, Error => Error);
      if not Error then
         if Last < S'Last
            and then (S (Last + 1) = '#' or else S (Last + 1) = ':')
         then
            Mark := S (Last + 1);
            Last := Last + 1;
            if Result = truncl (Result)
               and then Result in
                  Long_Long_Float (Formatting.Number_Base'First) ..
                  Long_Long_Float (Formatting.Number_Base'Last)
            then
               Base := Formatting.Number_Base (Result);
               Get_Unsigned_Real (S, Last, Result,
                  Base => Base,
                  Error => Error);
               if not Error then
                  if Last < S'Last and then S (Last + 1) = Mark then
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
         Val_Uns.Get_Exponent (S, Last, Exponent,
            Positive_Only => False,
            Error => Error);
         if not Error then
            if Exponent /= 0 then
               Result := Result * Long_Long_Float (Base) ** Exponent;
            end if;
            Result := copysignl (Result, Sign);
         end if;
      end if;
   end Get_Float_Literal;

end System.Val_Real;
