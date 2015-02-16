pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Val_LLD is
   pragma Pure;

   --  required for Fixed'Value by compiler (s-valdec.ads)
   function Value_Long_Long_Decimal (Str : String; Scale : Integer)
      return Long_Long_Integer;

end System.Val_LLD;
