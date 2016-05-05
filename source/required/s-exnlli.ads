pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Exponentiations;
package System.Exn_LLI is
   pragma Pure;

   --  required for "**" without checking by compiler (s-exnlli.ads)
   function Exn_Long_Long_Integer is
      new Exponentiations.Generic_Exp_Integer_No_Check (Long_Long_Integer);

end System.Exn_LLI;
