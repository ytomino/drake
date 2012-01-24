pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Float_Attributes is
   pragma Pure;

   function Machine_Rounding (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, Machine_Rounding, "__builtin_nearbyintl");

   function Remainder (X, Y : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, Remainder, "__builtin_remainderl");

   function Unbiased_Rounding (X : Long_Long_Float) return Long_Long_Float;

end System.Long_Long_Float_Attributes;
