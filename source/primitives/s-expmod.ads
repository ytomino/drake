pragma License (Unrestricted);
--  implementation package required by compiler
with System.Exponentiations;
package System.Exp_Mod is
   pragma Pure;

   --  required for "**" by compiler (s-expmod.ads)
   --  modular type does not raise exceptions.
   function Exp_Modular is
      new Exponentiations.Generic_Exp_Modular (Integer);

end System.Exp_Mod;
