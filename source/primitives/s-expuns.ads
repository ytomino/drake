pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
package System.Exp_Uns is
   pragma Pure;

   --  required for "**" with checking by compiler (s-expuns.ads)
   function Exp_Unsigned (
      Left : Unsigned_Types.Unsigned;
      Right : Natural)
      return Unsigned_Types.Unsigned;

end System.Exp_Uns;
