with C.stdlib;
pragma Warnings (Off, C.stdlib); -- break "pure" rule
package body System.Long_Long_Integer_Divisions is
   pragma Suppress (All_Checks);

   procedure Divide (
      Left, Right : Long_Long_Integer;
      Quotient, Remainder : out Long_Long_Integer)
   is
      Result : constant C.stdlib.lldiv_t := C.stdlib.lldiv (
         C.signed_long_long (Left),
         C.signed_long_long (Right));
   begin
      Quotient := Long_Long_Integer (Result.quot);
      Remainder := Long_Long_Integer (Result.F_rem);
   end Divide;

end System.Long_Long_Integer_Divisions;
