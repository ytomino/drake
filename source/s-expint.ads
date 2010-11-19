pragma License (Unrestricted);
--  implementation package required by compiler
package System.Exp_Int is
   pragma Pure;

   --  required for "**" with checking by compiler (s-expint.ads)
   function Exp_Integer (Left : Integer; Right : Natural) return Integer;

end System.Exp_Int;
