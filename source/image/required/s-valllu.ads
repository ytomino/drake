pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
package System.Val_LLU is
   pragma Pure;

   --  required for Modular'Value by compiler (s-valllu.ads)
   function Value_Long_Long_Unsigned (Str : String)
      return Unsigned_Types.Long_Long_Unsigned;

end System.Val_LLU;
