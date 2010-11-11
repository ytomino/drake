pragma License (Unrestricted);
--  implementation package required by compiler
with Interfaces;
package System.Arith_64 is
   pragma Preelaborate; --  depending C library

   --  required for multiplication with Overflow_Check by compiler (arit64.c)
   function Multiply (X, Y : Interfaces.Integer_64)
      return Interfaces.Integer_64;
   pragma Export (C, Multiply, "__gnat_mulv64");

   --  required for fixed-decimal division by compiler (s-arit64.ads)
   procedure Scaled_Divide (
      X, Y, Z : Interfaces.Integer_64; --  X * Y / Z
      Q, R : out Interfaces.Integer_64;
      Round : Boolean);

end System.Arith_64;
