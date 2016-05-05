pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Exponentiations;
package System.Exp_Int is
   pragma Pure;

   --  required for "**" with checking by compiler (s-expint.ads)
   function Exp_Integer is new Exponentiations.Generic_Exp_Integer (Integer);

end System.Exp_Int;
