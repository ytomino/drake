package body System.Long_Long_Elementary_Functions is

   function Fast_Log (X : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float (Fast_Log (Long_Float (X)));
   end Fast_Log;

   function Fast_Exp (X : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float (Fast_Exp (Long_Float (X)));
   end Fast_Exp;

   function Fast_Pow (Left, Right : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float (
         Fast_Pow (Long_Float (Left), Long_Float (Right)));
   end Fast_Pow;

   function Fast_Sinh (X : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float (Fast_Sinh (Long_Float (X)));
   end Fast_Sinh;

   function Fast_Cosh (X : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float (Fast_Cosh (Long_Float (X)));
   end Fast_Cosh;

   function Fast_Tanh (X : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float (Fast_Tanh (Long_Float (X)));
   end Fast_Tanh;

   function Fast_Arcsinh (X : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float (Fast_Arcsinh (Long_Float (X)));
   end Fast_Arcsinh;

   function Fast_Arccosh (X : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float (Fast_Arccosh (Long_Float (X)));
   end Fast_Arccosh;

   function Fast_Arctanh (X : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float (Fast_Arctanh (Long_Float (X)));
   end Fast_Arctanh;

end System.Long_Long_Elementary_Functions;
