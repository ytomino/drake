pragma License (Unrestricted);
--  implementation package required by compiler
package System.Exn_LLI is
   pragma Pure;

   --  required for "**" without checking by compiler (s-exnlli.ads)
   function Exn_Long_Long_Integer (Left : Long_Long_Integer; Right : Natural)
      return Long_Long_Integer;

end System.Exn_LLI;
