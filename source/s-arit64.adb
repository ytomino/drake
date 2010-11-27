with System.Long_Long_Integer_Divide;
with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising);
package body System.Arith_64 is
   pragma Suppress (All_Checks);
   use type Interfaces.Integer_64;

   pragma Compile_Time_Error (
      Long_Long_Integer'Size /= 64,
      "Long_Long_Integer is not 64-bit.");

   function Multiply_Overflow (X, Y : Interfaces.Integer_64) return Boolean;
   function Multiply_Overflow (X, Y : Interfaces.Integer_64) return Boolean is
   begin
      if X >= 0 then
         if Y >= 0 then
            return X > Interfaces.Integer_64'Last / Y;
         else
            return X < Interfaces.Integer_64'First / Y;
         end if;
      else
         if Y >= 0 then
            return Y < Interfaces.Integer_64'First / X;
         else
            return -X > Interfaces.Integer_64'Last / (-Y)
               or else X = Interfaces.Integer_64'First
               or else Y = Interfaces.Integer_64'First;
         end if;
      end if;
   end Multiply_Overflow;

   function GCD (A, B : Interfaces.Integer_64) return Interfaces.Integer_64;
   function GCD (A, B : Interfaces.Integer_64) return Interfaces.Integer_64 is
   begin
      if B = 0 then
         return A;
      else
         return GCD (B, A rem B);
      end if;
   end GCD;

   function Multiply (X, Y : Interfaces.Integer_64)
      return Interfaces.Integer_64 is
   begin
      if Multiply_Overflow (X, Y) then
         Unwind.Raising.Overflow;
      end if;
      return X * Y;
   end Multiply;

   procedure Scaled_Divide (
      X, Y, Z : Interfaces.Integer_64;
      Q, R : out Interfaces.Integer_64;
      Round : Boolean)
   is
      X2 : Interfaces.Integer_64 := X;
      Y2 : Interfaces.Integer_64 := Y;
      Z2 : Interfaces.Integer_64 := Z;
      M2 : Interfaces.Integer_64;
   begin
      if Z2 = 0 then
         Unwind.Raising.Zero_Division;
      end if;
      --  prime numbers may cause error... it should op 128-bit...
      declare
         M : constant Interfaces.Integer_64 := GCD (X2, Z2);
      begin
         X2 := X2 / M;
         Z2 := Z2 / M;
         M2 := M;
      end;
      declare
         M : constant Interfaces.Integer_64 := GCD (Y2, Z2);
      begin
         Y2 := Y2 / M;
         Z2 := Z2 / M;
         M2 := M2 * M;
      end;
      Long_Long_Integer_Divide (
         Long_Long_Integer (Multiply (X2, Y2)),
         Long_Long_Integer (Z2),
         Long_Long_Integer (Q),
         Long_Long_Integer (R));
      R := Multiply (R, M2);
      if Round then
         pragma Assert (R >= 0); --  ???
         if R >= abs Z / 2 then
            Q := Q + 1;
         end if;
      end if;
   end Scaled_Divide;

   procedure Double_Divide (
      X, Y, Z : Interfaces.Integer_64;
      Q, R : out Interfaces.Integer_64;
      Round : Boolean) is
   begin
      if Y = 0 or else Z = 0 then
         Unwind.Raising.Zero_Division;
      end if;
      if Multiply_Overflow (Y, Z) then
         Q := 0;
         R := X;
      else
         declare
            YZ : constant Interfaces.Integer_64 := Y * Z;
         begin
            Long_Long_Integer_Divide (
               Long_Long_Integer (X),
               Long_Long_Integer (YZ),
               Long_Long_Integer (Q),
               Long_Long_Integer (R));
            if Round then
               pragma Assert (R >= 0); --  ???
               if R > abs YZ / 2 then
                  Q := Q + 1;
               end if;
            end if;
         end;
      end if;
   end Double_Divide;

end System.Arith_64;
