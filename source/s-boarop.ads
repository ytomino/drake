pragma License (Unrestricted);
--  implementation package required by compiler
with System.Storage_Elements;
package System.Boolean_Array_Operations is
   pragma Pure;

   --  required for "not" unpacked boolean array by compiler (s-boarop.ads)
   procedure Vector_Not (
      R, X : Address;
      Length : Storage_Elements.Storage_Count);

   --  required for "and" unpacked boolean arrays by compiler (s-boarop.ads)
   procedure Vector_And (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count);

   --  required for "or" unpacked boolean arrays by compiler (s-boarop.ads)
   procedure Vector_Or (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count);

   --  required for "xor" unpacked boolean arrays by compiler (s-boarop.ads)
   procedure Vector_Xor (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count);

end System.Boolean_Array_Operations;
