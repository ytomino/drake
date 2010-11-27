pragma License (Unrestricted);
--  implementation package required by compiler
with System.Formatting;
package System.Val_Real is
   pragma Pure;

   --  required for Float'Value by compiler (s-valrea.ads)
   function Value_Real (Str : String) return Long_Long_Float;

   --  helper
   procedure Get_Unsigned_Real (
      S : String;
      Index : in out Positive;
      Result : out Long_Long_Float;
      Base : Formatting.Base_Type);

end System.Val_Real;
