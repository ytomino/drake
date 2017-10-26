pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Integer_Types is
   pragma Pure;

   --  Word size types

   type Word_Integer is range
      -(2 ** (Standard'Word_Size - 1)) .. 2 ** (Standard'Word_Size - 1) - 1;
   for Word_Integer'Size use Standard'Word_Size;

   subtype Word_Natural is Word_Integer range 0 .. Word_Integer'Last;
   subtype Word_Positive is Word_Integer range 1 .. Word_Integer'Last;

   type Word_Unsigned is mod 2 ** Standard'Word_Size;
   for Word_Unsigned'Size use Standard'Word_Size;

   pragma Provide_Shift_Operators (Word_Unsigned);

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
