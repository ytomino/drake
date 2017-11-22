pragma License (Unrestricted);
--  extended unit
package Ada.Float is
   --  Ada.Decimal-like (and more) utilities for float types.
   pragma Pure;

   generic
      type Float_Type is digits <>;
   function Infinity return Float_Type;
   pragma Inline (Infinity);

   generic
      type Float_Type is digits <>;
   function NaN return Float_Type;
   pragma Inline (NaN);

   generic
      type Float_Type is digits <>;
   function Is_Infinity (X : Float_Type) return Boolean;
   pragma Inline (Is_Infinity);

   generic
      type Float_Type is digits <>;
   function Is_NaN (X : Float_Type) return Boolean;
   pragma Inline (Is_NaN);

   generic
      type Float_Type is digits <>;
   function Is_Negative (X : Float_Type) return Boolean
      with Import, Convention => Intrinsic;

   generic
      type Dividend_Type is digits <>;
      type Divisor_Type is digits <>;
      type Quotient_Type is digits <>;
      type Remainder_Type is digits <>;
   procedure Divide (
      Dividend : Dividend_Type;
      Divisor : Divisor_Type;
      Quotient : out Quotient_Type;
      Remainder : out Remainder_Type);
   pragma Inline (Divide);

   generic
      type Dividend_Type is digits <>;
      type Quotient_Type is digits <>;
      type Remainder_Type is digits <>;
   procedure Divide_By_1 (
      Dividend : Dividend_Type;
      Quotient : out Quotient_Type;
      Remainder : out Remainder_Type); -- sign of Remainder = sign of Dividend
   pragma Inline (Divide_By_1);

   generic
      type Dividend_Type is digits <>;
      type Quotient_Type is digits <>;
      type Remainder_Type is digits <>;
   procedure Modulo_Divide_By_1 (
      Dividend : Dividend_Type;
      Quotient : out Quotient_Type;
      Remainder : out Remainder_Type); -- Remainder >= 0
   pragma Inline (Modulo_Divide_By_1);

end Ada.Float;
