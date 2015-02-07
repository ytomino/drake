pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Val_Int is
   pragma Pure;

   --  required for Integer'Value by compiler (s-valint.ads)
   function Value_Integer (Str : String) return Integer;

end System.Val_Int;
