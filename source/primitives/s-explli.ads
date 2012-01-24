pragma License (Unrestricted);
--  implementation package required by compiler
package System.Exp_LLI is
   pragma Pure;

   --  required for "**" with checking by compiler (s-explli.ads)
   function Exp_Long_Long_Integer (Left : Long_Long_Integer; Right : Natural)
      return Long_Long_Integer;

end System.Exp_LLI;
