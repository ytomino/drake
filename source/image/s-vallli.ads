pragma License (Unrestricted);
--  implementation package required by compiler
package System.Val_LLI is
   pragma Pure;

   --  required for Long_Long_Integer'Value by compiler (s-vallli.ads)
   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer;

   --  helper
   procedure Get_Long_Long_Integer_Literal (
      S : String;
      Last : out Natural;
      Result : out Long_Long_Integer;
      Error : out Boolean);

end System.Val_LLI;
