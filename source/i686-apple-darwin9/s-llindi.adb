with C.stdlib;
pragma Warnings (Off, C.stdlib);
procedure System.Long_Long_Integer_Divide (
   Left, Right : Long_Long_Integer;
   Quotient, Remainder : out Long_Long_Integer)
is
   pragma Suppress (All_Checks);
   Result : constant C.stdlib.lldiv_t := C.stdlib.lldiv (
      C.signed_long_long (Left),
      C.signed_long_long (Right));
begin
   Quotient := Long_Long_Integer (Result.quot);
   Remainder := Long_Long_Integer (Result.F_rem);
end System.Long_Long_Integer_Divide;
