with System.Long_Long_Elementary_Functions;
package body System.Long_Long_Complex_Types is
   pragma Suppress (All_Checks);

   function Fast_Argument (X : Complex) return Float is
   begin
      --  FreeBSD does not have cargf
      return Long_Long_Elementary_Functions.Fast_Arctan (X.Im, X.Re);
   end Fast_Argument;

   function Fast_Argument (X : Long_Complex) return Long_Float is
   begin
      --  FreeBSD does not have carg
      return Long_Long_Elementary_Functions.Fast_Arctan (X.Im, X.Re);
   end Fast_Argument;

   function Fast_Argument (X : Long_Long_Complex) return Long_Long_Float is
   begin
      --  FreeBSD does not have cargl
      return Long_Long_Elementary_Functions.Fast_Arctan (X.Im, X.Re);
   end Fast_Argument;

   function Fast_Modulus (X : Long_Long_Complex) return Long_Long_Float is
   begin
      --  FreeBSD does not have cabsl
      return Long_Long_Elementary_Functions.Fast_Sqrt (
         X.Re * X.Re + X.Im * X.Im);
   end Fast_Modulus;

end System.Long_Long_Complex_Types;
