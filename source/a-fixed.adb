with System.Long_Long_Float_Types;
with System.Long_Long_Integer_Types;
package body Ada.Fixed is
   use type System.Long_Long_Integer_Types.Longest_Unsigned;

   pragma Compile_Time_Error (
      Long_Long_Float'Size < 96,
      "Long_Long_Float is too short for the intermediate format.");

   --  implementation

   procedure Divide (
      Dividend : Dividend_Type;
      Divisor : Divisor_Type;
      Quotient : out Quotient_Type;
      Remainder : out Remainder_Type) is
   begin
      if Divisor = 0.0 and then Division_Check'Enabled then
         raise Constraint_Error;
      end if;
      if Dividend_Type'Small = Divisor_Type'Small then
         declare
            subtype LLU is System.Long_Long_Integer_Types.Longest_Unsigned;
            N : constant LLU :=
               LLU'Integer_Value (Dividend_Type'(abs Dividend));
            D : constant LLU := LLU'Integer_Value (Divisor_Type'(abs Divisor));
            Q : LLU;
            R : LLU;
         begin
            System.Long_Long_Integer_Types.Divide (N, D, Q, R);
            Quotient := Quotient_Type (Q);
            Remainder := Remainder_Type (Dividend_Type'Fixed_Value (R));
         end;
      elsif Dividend_Type'Small >= 1.0 and then Divisor_Type'Small >= 1.0 then
         declare
            subtype LLF is Long_Long_Float;
            N : constant LLF := LLF (abs Dividend);
            D : constant LLF := LLF (abs Divisor);
            Q : LLF;
            R : LLF;
         begin
            System.Long_Long_Float_Types.Divide (N, D, Q, R);
            Quotient := Quotient_Type (Q);
            Remainder := Remainder_Type (R);
         end;
      else
         declare
            subtype LLF is Long_Long_Float;
            ND_Smallest : constant LLF :=
               LLF'Min (Dividend_Type'Small, Divisor_Type'Small);
            N : constant LLF :=
               (Dividend_Type'Small / ND_Smallest)
               * LLF (
                  Long_Long_Integer'Integer_Value (
                     Dividend_Type'(abs Dividend)));
            D : constant LLF :=
               (Divisor_Type'Small / ND_Smallest)
               * LLF (
                  Long_Long_Integer'Integer_Value (
                     Divisor_Type'(abs Divisor)));
            Q : LLF;
            R : LLF;
         begin
            System.Long_Long_Float_Types.Divide (N, D, Q, R);
            Quotient := Quotient_Type (Q);
            Remainder := Remainder_Type (ND_Smallest * R);
         end;
      end if;
      if (Dividend < 0.0) /= (Divisor < 0.0) then
         Quotient := -Quotient;
      end if;
      if Dividend < 0.0 then
         Remainder := -Remainder;
      end if;
   end Divide;

end Ada.Fixed;
