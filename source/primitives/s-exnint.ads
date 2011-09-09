pragma License (Unrestricted);
--  implementation package required by compiler
package System.Exn_Int is
   pragma Pure;

   --  required for "**" without checking by compiler (s-exnint.ads)
   function Exn_Integer (Left : Integer; Right : Natural) return Integer;

end System.Exn_Int;
