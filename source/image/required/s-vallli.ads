pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Val_LLI is
   pragma Pure;

   --  required for Long_Long_Integer'Value by compiler (s-vallli.ads)
   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer;

end System.Val_LLI;
