with System.Long_Long_Integer_Divide;
with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising); -- break "pure" rule
package body System.Arith_64 is
   pragma Suppress (All_Checks);
   use type Interfaces.Integer_64;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   pragma Compile_Time_Error (
      Long_Long_Integer'Size /= 64,
      "Long_Long_Integer is not 64-bit.");

   function Multiply_Overflow (X, Y : Interfaces.Integer_64) return Boolean;
   function Multiply_Overflow (X, Y : Interfaces.Integer_64) return Boolean is
   begin
      if X > 0 then
         if Y > 0 then
            return X > Interfaces.Integer_64'Last / Y; -- Y > 0
         else
            return Y < Interfaces.Integer_64'First / X; -- X > 0
         end if;
      else
         if Y > 0 then
            return X < Interfaces.Integer_64'First / Y; -- Y > 0
         elsif Y < 0 then
            return -X > Interfaces.Integer_64'Last / (-Y) -- (-Y) > 0
               or else X = Interfaces.Integer_64'First
               or else Y = Interfaces.Integer_64'First;
         else
            return False; -- Y = 0
         end if;
      end if;
   end Multiply_Overflow;

   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;

   procedure Add (X : U64; R1, R2, R3 : in out U32);
   procedure Add (X : U64; R1, R2, R3 : in out U32) is
      R : constant U64 := U64 (R1) or Interfaces.Shift_Left (U64 (R2), 32);
      Result : constant U64 := X + R;
   begin
      R1 := U32'Mod (Result);
      R2 := U32'Mod (Interfaces.Shift_Right (Result, 32));
      if X > U64'Last - R then
         R3 := R3 + 1;
      end if;
   end Add;

   procedure Mul (X : U64; Y : U64; RL, RH : out U64);
   procedure Mul (X : U64; Y : U64; RL, RH : out U64) is
      X1 : constant U32 := U32'Mod (X);
      X2 : constant U32 := U32'Mod (Interfaces.Shift_Right (X, 32));
      Y1 : constant U32 := U32'Mod (Y);
      Y2 : constant U32 := U32'Mod (Interfaces.Shift_Right (Y, 32));
      R1, R2, R3, R4 : U32;
   begin
      declare
         R12 : constant U64 := U64 (X1) * U64 (Y1);
      begin
         R1 := U32'Mod (R12);
         R2 := U32'Mod (Interfaces.Shift_Right (R12, 32));
      end;
      if X2 = 0 and then Y2 = 0 then
         R3 := 0;
         R4 := 0;
      elsif X2 = 0 then
         R3 := 0;
         R4 := 0;
         declare
            T23 : constant U64 := U64 (X1) * U64 (Y2);
         begin
            Add (T23, R2, R3, R4);
         end;
      elsif Y2 = 0 then
         R3 := 0;
         R4 := 0;
         declare
            T23 : constant U64 := U64 (X2) * U64 (Y1);
         begin
            Add (T23, R2, R3, R4);
         end;
      else
         declare
            R34 : constant U64 := U64 (X2) * U64 (Y2);
         begin
            R3 := U32'Mod (R34);
            R4 := U32'Mod (Interfaces.Shift_Right (R34, 32));
         end;
         declare
            T23 : constant U64 := U64 (X1) * U64 (Y2);
         begin
            Add (T23, R2, R3, R4);
         end;
         declare
            T23 : constant U64 := U64 (X2) * U64 (Y1);
         begin
            Add (T23, R2, R3, R4);
         end;
      end if;
      RL := U64 (R1) or Interfaces.Shift_Left (U64 (R2), 32);
      RH := U64 (R3) or Interfaces.Shift_Left (U64 (R4), 32);
   end Mul;

   procedure Div (X : U64; Y : U64; Q : out U64; R : out U64);
   procedure Div (X : U64; Y : U64; Q : out U64; R : out U64) is
   begin
      Long_Long_Integer_Divide (
         Long_Long_Integer (X),
         Long_Long_Integer (Y),
         Long_Long_Integer (Q),
         Long_Long_Integer (R));
   end Div;

   function Bits (X : U64) return Natural;
   function Bits (X : U64) return Natural is
      T : U64 := X;
   begin
      return Result : Natural := 0 do
         while T > 0 loop
            Result := Result + 1;
            T := T / 2;
         end loop;
      end return;
   end Bits;

   procedure Div (XL, XH : U64; Y : U64; Q : out U64; R : out U64);
   procedure Div (XL, XH : U64; Y : U64; Q : out U64; R : out U64) is
      --  pragma Assert (XH < Y);
      Temp_XL : U64 := XL;
      Temp_XH : U64 := XH;
      Rem_Q : U64;
   begin
      Q := 0;
      while Temp_XH > 0 or else Temp_XL > U64 (Interfaces.Integer_64'Last) loop
         declare
            XH_Bits : constant Natural := Bits (Temp_XH) + 1;
            --  "+1" means not using sign bit
            --  since Long_Long_Integer_Divide is signed
            Scaled_X : constant U64 :=
               Interfaces.Shift_Left (Temp_XH, 64 - XH_Bits) or
               Interfaces.Shift_Right (Temp_XL, XH_Bits);
            --  bits of Scaled_X = 63
         begin
            if Scaled_X < Y then
               --  bits of Y = 63 (original Y is signed)
               declare
                  Shift : constant Natural := XH_Bits - 1;
                  YL : constant U64 := Interfaces.Shift_Left (Y, Shift);
                  YH : constant U64 := Interfaces.Shift_Right (Y, 64 - Shift);
               begin
                  if Temp_XL < YL then
                     Temp_XH := Temp_XH - 1; -- borrow
                  end if;
                  Temp_XL := Temp_XL - YL;
                  Temp_XH := Temp_XH - YH;
               end;
            else
               declare
                  Temp_Q, Temp_R : U64;
               begin
                  Div (Scaled_X, Y, Temp_Q, Temp_R);
                  Q := Q + Interfaces.Shift_Left (Temp_Q, XH_Bits);
                  Temp_XH := Interfaces.Shift_Right (Temp_R, 64 - XH_Bits);
                  Temp_XL := Interfaces.Shift_Left (Temp_R, XH_Bits) or
                     (Temp_XL and (Interfaces.Shift_Left (1, XH_Bits) - 1));
               end;
            end if;
         end;
      end loop;
      Div (Temp_XL, Y, Rem_Q, R);
      Q := Q + Rem_Q;
   end Div;

   --  implementation

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
      Round : Boolean) is
   begin
      if Z = 0 then
         Unwind.Raising.Zero_Division;
      end if;
      declare
         XY_Is_Minus : constant Boolean := (X < 0) xor (Y < 0);
         Q_Is_Minus : constant Boolean := XY_Is_Minus xor (Z < 0);
         AX : constant U64 := U64'Mod (abs X);
         AY : constant U64 := U64'Mod (abs Y);
         AZ : constant U64 := U64'Mod (abs Z);
         AXYL, AXYH : U64;
         AQ, AR : U64;
      begin
         Mul (AX, AY, AXYL, AXYH);
         if AXYH >= AZ then
            Unwind.Raising.Overflow;
         end if;
         Div (AXYL, AXYH, AZ, AQ, AR);
         if Round and then AR > (AZ - 1) / 2 then
            AQ := AQ + 1;
         end if;
         if Q_Is_Minus then
            Q := -Interfaces.Integer_64 (AQ);
         else
            Q := Interfaces.Integer_64 (AQ);
         end if;
         if XY_Is_Minus then
            R := -Interfaces.Integer_64 (AR);
         else
            R := Interfaces.Integer_64 (AR);
         end if;
      end;
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
            if Round and then U64 (abs R) > (U64 (abs YZ) - 1) / 2 then
               if Q > 0 then
                  Q := Q + 1;
               else
                  Q := Q - 1;
               end if;
            end if;
         end;
      end if;
   end Double_Divide;

end System.Arith_64;
