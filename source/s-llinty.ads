pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Integer_Types is
   pragma Pure;

   --  Largest types

   pragma Compile_Time_Error (
      Max_Binary_Modulus /= 2 ** Long_Long_Integer'Size,
      "Long_Long_Integer is not largest type.");

   type Long_Long_Unsigned is mod 2 ** Long_Long_Integer'Size;
   for Long_Long_Unsigned'Size use Long_Long_Integer'Size;

   pragma Provide_Shift_Operators (Long_Long_Unsigned);

   procedure Divide (
      Left, Right : Long_Long_Unsigned;
      Quotient, Remainder : out Long_Long_Unsigned);
   pragma Inline (Divide);

end System.Long_Long_Integer_Types;
