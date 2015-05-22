pragma License (Unrestricted);
package Ada.Decimal is
   pragma Pure;

   Max_Scale : constant := +18; -- implementation-defined
   Min_Scale : constant := -18; -- implementation-defined

   Min_Delta : constant := 10.0 ** (-Max_Scale);
   Max_Delta : constant := 10.0 ** (-Min_Scale);

   Max_Decimal_Digits : constant := 18; -- implementation-defined

   generic
      type Dividend_Type is delta <> digits <>;
      type Divisor_Type is delta <> digits <>;
      type Quotient_Type is delta <> digits <>;
      type Remainder_Type is delta <> digits <>;
   procedure Divide (
      Dividend : Dividend_Type;
      Divisor : Divisor_Type;
      Quotient : out Quotient_Type;
      Remainder : out Remainder_Type)
      with Import, Convention => Intrinsic;

end Ada.Decimal;
