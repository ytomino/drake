pragma License (Unrestricted);
--  implementation package required by compiler
package System.Exn_LLF is
   pragma Pure;

   --  required for "**" without checking by compiler (s-exnllf.ads)
   function Exn_Long_Long_Float (Left : Long_Long_Float; Right : Integer)
      return Long_Long_Float;
   pragma Import (Intrinsic, Exn_Long_Long_Float, "__builtin_powil");

end System.Exn_LLF;
