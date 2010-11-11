with System.Unwind.Raising;
with C.stdlib;
package body System.Arith_64 is
   pragma Suppress (All_Checks);
   use type Interfaces.Integer_64;

   pragma Compile_Time_Error (
      C.signed_long_long'Size /= 64,
      "long long is not 64-bit.");

   function Multiply (X, Y : Interfaces.Integer_64)
      return Interfaces.Integer_64 is
   begin
      if X >= 0 then
         if Y >= 0 then
            if X > Interfaces.Integer_64'Last / Y then
               Unwind.Raising.Overflow;
            end if;
         else
            if X < Interfaces.Integer_64'First / Y then
               Unwind.Raising.Overflow;
            end if;
         end if;
      else
         if Y >= 0 then
            if Y < Interfaces.Integer_64'First / X then
               Unwind.Raising.Overflow;
            end if;
         else
            if -X > Interfaces.Integer_64'Last / (-Y)
               or else X = Interfaces.Integer_64'First
               or else Y = Interfaces.Integer_64'First
            then
               Unwind.Raising.Overflow;
            end if;
         end if;
      end if;
      return X * Y;
   end Multiply;

   procedure Scaled_Divide (
      X, Y, Z : Interfaces.Integer_64;
      Q, R : out Interfaces.Integer_64;
      Round : Boolean)
   is
      --  too very simple implementation... to be replaced
      pragma Unreferenced (Round);
      pragma Unsuppress (Overflow_Check);
      XY : constant Interfaces.Integer_64 := X * Y;
      pragma Suppress (Overflow_Check);
   begin
      declare
         Result : constant C.stdlib.lldiv_t := C.stdlib.lldiv (
            C.signed_long_long (XY),
            C.signed_long_long (Z));
      begin
         Q := Interfaces.Integer_64 (Result.quot);
         R := Interfaces.Integer_64 (Result.F_rem);
      end;
   end Scaled_Divide;

end System.Arith_64;
