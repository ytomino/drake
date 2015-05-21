with System.Long_Long_Integer_Divisions;
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

   procedure unreachable
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_unreachable";
   pragma No_Return (unreachable);

   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;

   function add_overflow (
      a, b : U64;
      res : not null access U64)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__builtin_uaddll_overflow";

   function mul_overflow (
      a, b : Interfaces.Integer_64;
      res : not null access Interfaces.Integer_64)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__builtin_smulll_overflow";

   procedure Add (X : U64; R1, R2, R3 : in out U32);
   procedure Add (X : U64; R1, R2, R3 : in out U32) is
      R : constant U64 := U64 (R1) or Interfaces.Shift_Left (U64 (R2), 32);
      Result : aliased U64;
      Overflow : Boolean;
   begin
      Overflow := add_overflow (X, R, Result'Access);
      R1 := U32'Mod (Result);
      R2 := U32'Mod (Interfaces.Shift_Right (Result, 32));
      if Overflow then
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
      Long_Long_Integer_Divisions.Divide (
         Long_Long_Integer_Divisions.Longest_Unsigned (X),
         Long_Long_Integer_Divisions.Longest_Unsigned (Y),
         Long_Long_Integer_Divisions.Longest_Unsigned (Q),
         Long_Long_Integer_Divisions.Longest_Unsigned (R));
   end Div;

   type Unsigned is mod 2 ** Integer'Size;

   function clz (X : U64) return Unsigned;
   pragma Import (Intrinsic, clz, "__builtin_clzll");

   procedure Div (XL, XH : U64; Y : U64; Q : out U64; R : out U64);
   procedure Div (XL, XH : U64; Y : U64; Q : out U64; R : out U64) is
      Temp_XL : U64 := XL;
      Temp_XH : U64 := XH; -- XH <= 2 ** 62 (abs signed * abs signed)
      Rem_Q : U64;
   begin
      Q := 0;
      while Temp_XH > 0 loop
         declare
            Scaling_W : constant Natural := Natural (clz (Temp_XH) xor 63) + 1;
            Scaled_X : U64;
         begin
            if Scaling_W not in 1 .. 63 then
               unreachable;
            end if;
            Scaled_X :=
               Interfaces.Shift_Left (Temp_XH, 64 - Scaling_W) or
               Interfaces.Shift_Right (Temp_XL, Scaling_W);
            --  0 < Scaled_X < 2 ** 63
            if Scaled_X < Y then
               --  Y <= 2 ** 63 (original Y is signed)
               declare
                  YL : constant U64 := Interfaces.Shift_Left (Y, Scaling_W);
                  YH : constant U64 :=
                     Interfaces.Shift_Right (Y, 64 - Scaling_W);
               begin
                  Q := Q + Interfaces.Shift_Left (1, Scaling_W);
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
                  Q := Q + Interfaces.Shift_Left (Temp_Q, Scaling_W);
                  Temp_XH := Interfaces.Shift_Right (Temp_R, 64 - Scaling_W);
                  Temp_XL := Interfaces.Shift_Left (Temp_R, Scaling_W) or
                     (Temp_XL and (Interfaces.Shift_Left (1, Scaling_W) - 1));
               end;
            end if;
         end;
      end loop;
      Div (Temp_XL, Y, Rem_Q, R);
      Q := Q + Rem_Q;
   end Div;

   --  implementation

   function Multiply (X, Y : Interfaces.Integer_64)
      return Interfaces.Integer_64
   is
      Result : aliased Interfaces.Integer_64;
   begin
      if mul_overflow (X, Y, Result'Access) then
         Unwind.Raising.Overflow;
      end if;
      return Result;
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
         XY_Is_Minus : constant Boolean := (X < 0) /= (Y < 0);
         Q_Is_Minus : constant Boolean := XY_Is_Minus /= (Z < 0);
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
            if AQ > 2 ** 63 then
               Unwind.Raising.Overflow;
            end if;
            Q := -Interfaces.Integer_64 (AQ);
         else
            if AQ >= 2 ** 63 then
               Unwind.Raising.Overflow;
            end if;
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
      Round : Boolean)
   is
      YZ : aliased Interfaces.Integer_64;
   begin
      if Y = 0 or else Z = 0 then
         Unwind.Raising.Zero_Division;
      end if;
      if mul_overflow (Y, Z, YZ'Access) then
         if X = Interfaces.Integer_64'First
            and then (Y = -1 or else Z = -1)
         then -- special case, Integer_64'First / (Integer_64'First * -1)
            Q := -1;
            R := 0;
         else
            Q := 0;
            R := X;
         end if;
      else
         declare
            AX : constant U64 := U64'Mod (abs X);
            AYZ : constant U64 := U64'Mod (abs YZ);
            AQ, AR : U64;
         begin
            Div (AX, AYZ, AQ, AR);
            if Round and then AR > (AYZ - 1) / 2 then
               AQ := AQ + 1;
            end if;
            if (X < 0) /= (YZ <= 0) then
               if AQ > 2 ** 63 then
                  Unwind.Raising.Overflow;
               end if;
               Q := -Interfaces.Integer_64 (AQ);
            else
               if AQ >= 2 ** 63 then
                  Unwind.Raising.Overflow;
               end if;
               Q := Interfaces.Integer_64 (AQ);
            end if;
            if X < 0 then
               R := -Interfaces.Integer_64 (AR);
            else
               R := Interfaces.Integer_64 (AR);
            end if;
         end;
      end if;
   end Double_Divide;

end System.Arith_64;
