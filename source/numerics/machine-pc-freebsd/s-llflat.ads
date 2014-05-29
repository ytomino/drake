pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Float_Attributes is
   pragma Pure;

   function Machine_Rounding (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, Machine_Rounding, "__builtin_roundl");
   --  FreeBSD does not have nearbyintl, it use roundl instead of nearbyintl.
   --  it should be replaced by inline-assember for depending on rounding mode?

   function Remainder (X, Y : Long_Long_Float) return Long_Long_Float;

   function Unbiased_Rounding (X : Long_Long_Float) return Long_Long_Float;

end System.Long_Long_Float_Attributes;
