pragma License (Unrestricted);
--  implementation unit required by compiler
with Interfaces;
package System.Arith_64 is
   pragma Pure;

   --  implementation for multiplication with Overflow_Check (arit64.c)
   function Multiply (X, Y : Interfaces.Integer_64)
      return Interfaces.Integer_64
      with Export, Convention => C, External_Name => "__gnat_mulv64";

   --  required for fixed-decimal (X * Y) / Z by compiler (s-arit64.ads)
   procedure Scaled_Divide (
      X, Y, Z : Interfaces.Integer_64; -- X * Y / Z
      Q, R : out Interfaces.Integer_64;
      Round : Boolean);

   --  required for fixed-decimal X / (Y * Z) by compiler (s-arit64.ads)
   procedure Double_Divide (
      X, Y, Z : Interfaces.Integer_64;
      Q, R : out Interfaces.Integer_64;
      Round : Boolean);

   --  required if Long_Long_Integer'Size > 64 (s-arit64.ads)
--  function Add_With_Ovflo_Check (X, Y : Interfaces.Integer_64)
--    return Interfaces.Integer_64;
--  function Subtract_With_Ovflo_Check (X, Y : Interfaces.Integer_64)
--    return Interfaces.Integer_64;
--  function Multiply_With_Ovflo_Check (X, Y : Interfaces.Integer_64)
--    return Interfaces.Integer_64;

end System.Arith_64;
