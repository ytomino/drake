pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Aux_Dec is
   pragma Pure;

   --  required for T'Type_Class attribute by compiler (s-auxdec.ads)
   type Type_Class is (
      Type_Class_Enumeration,
      Type_Class_Integer,
      Type_Class_Fixed_Point,
      Type_Class_Floating_Point,
      Type_Class_Array,
      Type_Class_Record,
      Type_Class_Access,
      Type_Class_Task,
      Type_Class_Address);

private

   --  required for renaming 'Iamge attribute(?) by compiler (s-auxdec.ads)
   type AST_Handler is access procedure (Param : Long_Integer);

   --  required by compiler ??? (s-auxdec.ads)
--  No_AST_Handler : constant AST_Handler;
--  function Import_Value (Symbol : String) return Unsigned_32;

end System.Aux_Dec;
