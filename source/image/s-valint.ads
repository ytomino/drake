pragma License (Unrestricted);
--  implementation package required by compiler
package System.Val_Int is
   pragma Pure;

   --  required for Integer'Value by compiler (s-valint.ads)
   function Value_Integer (Str : String) return Integer;

   --  helper
   procedure Get_Integer_Literal (
      S : String;
      Last : out Natural;
      Result : out Integer);

end System.Val_Int;
