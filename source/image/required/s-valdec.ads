pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Val_Dec is
   pragma Pure;

   --  required for Fixed'Value by compiler (s-valdec.ads)
   function Value_Decimal (Str : String; Scale : Integer) return Integer;

end System.Val_Dec;
