pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
package System.Exp_Uns is
   pragma Pure;

   --  required for "**" by compiler (s-expuns.ads)
   --  modular type does not raise exceptions.
   function Exp_Unsigned (
      Left : Unsigned_Types.Unsigned;
      Right : Natural)
      return Unsigned_Types.Unsigned;

end System.Exp_Uns;
