pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Exponentiations;
package System.Exp_LLI is
   pragma Pure;

   function Shift_Left (Value : Long_Long_Integer; Amount : Natural)
      return Long_Long_Integer;
   pragma Import (Intrinsic, Shift_Left);

   --  required for "**" with checking by compiler (s-explli.ads)
   function Exp_Long_Long_Integer is
      new Exponentiations.Generic_Exp_Integer (
         Long_Long_Integer,
         Shift_Left => Shift_Left);

end System.Exp_LLI;
