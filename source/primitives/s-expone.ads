pragma License (Unrestricted);
--  implementation unit
package System.Exponentiations is
   pragma Pure;

   generic
      type Integer_Type is range <>;
      with function Shift_Left (Value : Integer_Type; Amount : Natural)
         return Integer_Type is <>;
   function Generic_Exp_Integer (Left : Integer_Type; Right : Natural)
      return Integer_Type;

   generic
      type Integer_Type is range <>;
      with function Shift_Left (Value : Integer_Type; Amount : Natural)
         return Integer_Type is <>;
   function Generic_Exp_Integer_No_Check (Left : Integer_Type; Right : Natural)
      return Integer_Type;

   --  Modulus = 2 ** N
   generic
      type Unsigned_Type is mod <>;
      with function Shift_Left (Value : Unsigned_Type; Amount : Natural)
         return Unsigned_Type is <>;
   function Generic_Exp_Unsigned (Left : Unsigned_Type; Right : Natural)
      return Unsigned_Type;

   --  Modulus /= 2 ** N
   generic
      type Integer_Type is range <>;
   function Generic_Exp_Modular (Left, Modulus : Integer_Type; Right : Natural)
      return Integer_Type;

end System.Exponentiations;
