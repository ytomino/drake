pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Exponentiations;
package System.Exp_Int is
   pragma Pure;

   function Shift_Left (Value : Integer; Amount : Natural)
      return Integer
      with Import, Convention => Intrinsic;

   --  required for "**" with checking by compiler (s-expint.ads)
   function Exp_Integer is
      new Exponentiations.Generic_Exp_Integer (
         Integer,
         Shift_Left => Shift_Left);

end System.Exp_Int;
