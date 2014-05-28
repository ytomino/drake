pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
package System.Val_Uns is
   pragma Pure;

   --  required for Modular'Value by compiler (s-valuns.ads)
   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned;

end System.Val_Uns;
