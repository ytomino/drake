pragma License (Unrestricted);
--  implementation package required by compiler
package System.Exp_Mod is
   pragma Pure;

   --  required for "**" by compiler (s-expmod.ads)
   --  modular type does not raise exceptions.
   function Exp_Modular (Left : Integer; Modulus : Integer; Right : Natural)
      return Integer;

end System.Exp_Mod;
