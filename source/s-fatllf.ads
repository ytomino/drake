pragma License (Unrestricted);
--  implementation package required by compiler
package System.Fat_LLF is
   pragma Pure;

   package Attr_Long_Long_Float is

      --  required for Long_Long_Float'Floor by compiler
      function Floor (X : Long_Long_Float) return Long_Long_Float;
      pragma Import (Intrinsic, Floor, "__builtin_floorl");

   end Attr_Long_Long_Float;

end System.Fat_LLF;
