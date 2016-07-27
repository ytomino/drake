pragma License (Unrestricted);
package Interfaces is
   pragma Pure;

   type Integer_8 is range -(2 ** 7) .. 2 ** 7 - 1; -- 2's complement
   for Integer_8'Size use 8;
   type Integer_16 is range -(2 ** 15) .. 2 ** 15 - 1;
   for Integer_16'Size use 16;
   type Integer_32 is range -(2 ** 31) .. 2 ** 31 - 1;
   for Integer_32'Size use 32;
   type Integer_64 is range -(2 ** 63) .. 2 ** 63 - 1;
   for Integer_64'Size use 64;

   type Unsigned_8 is mod 2 ** 8;
   for Unsigned_8'Size use 8;
   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;
   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;
   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;

--  function Shift_Left (Value : Unsigned_n; Amount : Natural)
--    return Unsigned_n;
--  function Shift_Right (Value : Unsigned_n; Amount : Natural)
--    return Unsigned_n;
--  function Shift_Right_Arithmetic (Value : Unsigned_n; Amount : Natural)
--    return Unsigned_n;
--  function Rotate_Left (Value : Unsigned_n; Amount : Natural)
--    return Unsigned_n;
--  function Rotate_Right (Value : Unsigned_n; Amount : Natural)
--    return Unsigned_n;
--  ...
   pragma Provide_Shift_Operators (Unsigned_8);
   pragma Provide_Shift_Operators (Unsigned_16);
   pragma Provide_Shift_Operators (Unsigned_32);
   pragma Provide_Shift_Operators (Unsigned_64);

end Interfaces;
