pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Exponentiations;
with System.Unsigned_Types;
package System.Exp_Mod is
   pragma Pure;

   --  required for "**" by compiler (s-expmod.ads)
   --  modular type does not raise exceptions.
   function Exp_Modular is
      new Exponentiations.Generic_Exp_Modular (
         Integer,
         System.Unsigned_Types.Unsigned,
         Shift_Left => System.Unsigned_Types.Shift_Left);

end System.Exp_Mod;
