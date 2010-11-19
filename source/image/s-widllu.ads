pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
package System.Wid_LLU is
   pragma Pure;

   --  required for Modular'Width by compiler (s-widllu.ads)
   function Width_Long_Long_Unsigned (
      Lo, Hi : Unsigned_Types.Long_Long_Unsigned)
      return Natural;

end System.Wid_LLU;
