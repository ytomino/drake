pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
package System.Exp_LLU is
   pragma Pure;

   --  required for "**" by compiler (s-expllu.ads)
   --  modular type does not raise exceptions.
   function Exp_Long_Long_Unsigned (
      Left : Unsigned_Types.Long_Long_Unsigned;
      Right : Natural)
      return Unsigned_Types.Long_Long_Unsigned;

end System.Exp_LLU;
