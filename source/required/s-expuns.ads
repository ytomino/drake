pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Unsigned_Types;
with System.Exponentiations;
package System.Exp_Uns is
   pragma Pure;

   --  required for "**" by compiler (s-expuns.ads)
   --  modular type does not raise exceptions.
   function Exp_Unsigned is
      new Exponentiations.Generic_Exp_Unsigned (
         Unsigned_Types.Unsigned,
         Shift_Left => Unsigned_Types.Shift_Left);

end System.Exp_Uns;
