pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
with System.Exponentiations;
package System.Exp_LLU is
   pragma Pure;

   --  required for "**" by compiler (s-expllu.ads)
   --  modular type does not raise exceptions.
   function Exp_Long_Long_Unsigned is
      new Exponentiations.Generic_Exp_Unsigned (
         Unsigned_Types.Long_Long_Unsigned,
         Shift_Left => Unsigned_Types.Shift_Left);

end System.Exp_LLU;
