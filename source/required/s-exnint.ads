pragma License (Unrestricted);
--  implementation package required by compiler
with System.Exponentiations;
package System.Exn_Int is
   pragma Pure;

   function Shift_Left (Value : Integer; Amount : Natural)
      return Integer;
   pragma Import (Intrinsic, Shift_Left);

   --  required for "**" without checking by compiler (s-exnint.ads)
   function Exn_Integer is
      new Exponentiations.Generic_Exp_Integer_No_Check (
         Integer,
         Shift_Left => Shift_Left);

end System.Exn_Int;
