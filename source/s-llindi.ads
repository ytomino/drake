pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Integer_Divisions is
   pragma Pure;

   type Longest_Unsigned is mod 2 ** Long_Long_Integer'Size;

   procedure Divide (
      Left, Right : Longest_Unsigned;
      Quotient, Remainder : out Longest_Unsigned);
   pragma Inline (Divide);

end System.Long_Long_Integer_Divisions;
