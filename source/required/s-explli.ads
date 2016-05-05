pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Exponentiations;
package System.Exp_LLI is
   pragma Pure;

   --  required for "**" with checking by compiler (s-explli.ads)
   function Exp_Long_Long_Integer is
      new Exponentiations.Generic_Exp_Integer (Long_Long_Integer);

end System.Exp_LLI;
