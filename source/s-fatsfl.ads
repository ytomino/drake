pragma License (Unrestricted);
--  implementation package required by compiler
package System.Fat_Sflt is
   pragma Pure;

   package Attr_Short_Float is

      --  required for Short_Float'Floor by compiler (s-fatgen.ads)
      function Floor (X : Short_Float) return Short_Float;
      pragma Import (Intrinsic, Floor, "__builtin_floorf");

      --  required for Short_Float'Valid by compiler (s-fatgen.ads)
      function Valid (X : not null access Short_Float) return Boolean;

   end Attr_Short_Float;

end System.Fat_Sflt;
