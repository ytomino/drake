with System.Long_Long_Elementary_Functions;
package body Ada.Numerics.Distributions is

   type Longest_Unsigned is mod 2 ** Long_Long_Integer'Size;

   function popcountll (x : Longest_Unsigned) return Integer
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_popcountll";

   --  Simple distributions

   function Linear_Discrete (X : Source) return Target is
      Source_W : constant Longest_Unsigned :=
         Source'Pos (Source'Last) - Source'Pos (Source'First);
      Target_W : constant Longest_Unsigned :=
         Target'Pos (Target'Last) - Target'Pos (Target'First);
   begin
      if Source_W = 0 or else Target_W = 0 then
         --  0 bit value
         return Target'First;
      elsif Source_W = Target_W then
         --  1:1 mapping
         return Target'Val (Longest_Unsigned (X) + Target'Pos (Target'First));
      elsif Longest_Unsigned'Max (Source_W, Target_W) < Longest_Unsigned'Last
         and then (Source_W + 1) * (Target_W + 1) >= Source_W
         and then (Source_W + 1) * (Target_W + 1) >= Target_W
      then
         --  no overflow
         return Target'Val (
            Longest_Unsigned (X) * (Target_W + 1) / (Source_W + 1)
            + Target'Pos (Target'First));
      else
         --  use Long_Long_Float
         declare
            function To_Float is
               new Linear_Float_0_To_Less_Than_1 (
                  Source,
                  Long_Long_Float);
         begin
            return Target'Val (
               Longest_Unsigned (
                  Long_Long_Float'Floor (
                     To_Float (X) * (Long_Long_Float (Target_W) + 1.0)))
               + Target'Pos (Target'First));
         end;
      end if;
   end Linear_Discrete;

   function Linear_Float_0_To_1 (X : Source) return Target'Base is
   begin
      return Target'Base (X) * Target'Base (1.0 / (Source'Modulus - 1));
   end Linear_Float_0_To_1;

   function Linear_Float_0_To_Less_Than_1 (X : Source) return Target'Base is
   begin
      return Target'Base (X) * Target'Base (1.0 / Source'Modulus);
   end Linear_Float_0_To_Less_Than_1;

   function Linear_Float_Greater_Than_0_To_Less_Than_1 (X : Source)
      return Target'Base is
   begin
      return (Target'Base (X) + 0.5) * Target'Base (1.0 / Source'Modulus);
   end Linear_Float_Greater_Than_0_To_Less_Than_1;

   function Exponentially_Float (X : Source) return Target'Base is
      subtype Float_Type is Target;
      function Fast_Log (X : Float_Type'Base) return Float_Type'Base;
      function Fast_Log (X : Float_Type'Base) return Float_Type'Base is
      begin
         if Float_Type'Digits <= Float'Digits then
            declare
               function logf (A1 : Float) return Float
                  with Import,
                     Convention => Intrinsic,
                     External_Name => "__builtin_logf";
            begin
               return Float_Type'Base (logf (Float (X)));
            end;
         elsif Float_Type'Digits <= Long_Float'Digits then
            declare
               function log (A1 : Long_Float) return Long_Float
                  with Import,
                     Convention => Intrinsic, External_Name => "__builtin_log";
            begin
               return Float_Type'Base (log (Long_Float (X)));
            end;
         else
            return Float_Type'Base (
               System.Long_Long_Elementary_Functions.Fast_Log (
                  Long_Long_Float (X)));
         end if;
      end Fast_Log;
      function Float_0_To_Less_Than_1 is
         new Linear_Float_0_To_Less_Than_1 (Source, Target'Base);
      Y : constant Target'Base := Float_0_To_Less_Than_1 (X); -- [0,1)
      Z : constant Target'Base := 1.0 - Y; -- (0,1]
   begin
      return -Fast_Log (Z);
   end Exponentially_Float;

   --  Simple distributions for random number

   function Linear_Discrete_Random (Gen : aliased in out Generator)
      return Target is
   begin
      if Target'First > Target'Last then
         raise Constraint_Error;
      end if;
      declare
         function To_Target is new Linear_Discrete (Source, Target);
      begin
         return To_Target (Get (Gen));
      end;
   end Linear_Discrete_Random;

   --  Strict uniform distributions for random number

   function Uniform_Discrete_Random (Gen : aliased in out Generator)
      return Target is
   begin
      if Target'First > Target'Last then
         raise Constraint_Error;
      end if;
      declare
         Source_W : constant Longest_Unsigned :=
            Source'Pos (Source'Last) - Source'Pos (Source'First);
         Target_W : constant Longest_Unsigned :=
            Target'Pos (Target'Last) - Target'Pos (Target'First);
      begin
         if Source_W = 0 or else Target_W = 0 then
            --  0 bit value
            return Target'First;
         elsif Source_W = Target_W then
            --  1:1 mapping
            declare
               X : constant Longest_Unsigned := Longest_Unsigned (Get (Gen));
            begin
               return Target'Val (X + Target'Pos (Target'First));
            end;
         elsif Source_W > Target_W
            and then (
               Source_W = Longest_Unsigned'Last
               or else popcountll (Source_W + 1) = 1)
            and then popcountll (Target_W + 1) = 1
         then
            --  narrow, and 2 ** n
            declare
               X : constant Longest_Unsigned := Longest_Unsigned (Get (Gen));
            begin
               return Target'Val (
                  X / (Source_W / (Target_W + 1) + 1)
                  + Target'Pos (Target'First));
            end;
         else
            loop
               declare
                  Max : Longest_Unsigned;
                  X : Longest_Unsigned;
               begin
                  if Source_W > Target_W then
                     --  narrow
                     Max := Source_W;
                     X := Longest_Unsigned (Get (Gen));
                  else
                     --  wide
                     Max := 0;
                     X := 0;
                     loop
                        declare
                           Old_Max : constant Longest_Unsigned := Max;
                        begin
                           Max := (Max * (Source_W + 1)) + Source_W;
                           X := (X * (Source_W + 1))
                              + Longest_Unsigned (Get (Gen));
                           exit when Max >= Target_W
                              or else Max <= Old_Max; -- overflow
                        end;
                     end loop;
                  end if;
                  if Target_W = Longest_Unsigned'Last then
                     --  Source'Range_Length < Target'RL = Longest_Unsigned'RL
                     return Target'Val (X + Target'Pos (Target'First));
                  else
                     declare
                        --  (Max + 1) mod (Target_W + 1)
                        R : constant Longest_Unsigned :=
                           (Max mod (Target_W + 1) + 1) mod (Target_W + 1);
                     begin
                        --  (Max - R + 1) mod (Target_W + 1) = 0
                        if R = 0 or else X <= Max - R then
                           return Target'Val (
                              X mod (Target_W + 1)
                                 + Target'Pos (Target'First));
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end if;
      end;
   end Uniform_Discrete_Random;

   function Uniform_Float_Random_0_To_1 (Gen : aliased in out Generator)
      return Target is
   begin
      if Target'Machine_Mantissa <= 24 then -- Float'Machine_Mantissa
         declare
            type Unsigned_24_plus_1 is range 0 .. 2 ** 24;
            function Unsigned_24_plus_1_Random is
               new Uniform_Discrete_Random (Source, Unsigned_24_plus_1,
                  Generator, Get);
            X : constant Unsigned_24_plus_1 := Unsigned_24_plus_1_Random (Gen);
         begin
            return Target'Base (X) * Target'Base (1.0 / 2 ** 24);
         end;
      elsif Target'Machine_Mantissa <= 53 then -- Long_Float'Machine_Mantissa
         declare
            type Unsigned_53_plus_1 is range 0 .. 2 ** 53;
            function Unsigned_53_plus_1_Random is
               new Uniform_Discrete_Random (Source, Unsigned_53_plus_1,
                  Generator, Get);
            X : constant Unsigned_53_plus_1 := Unsigned_53_plus_1_Random (Gen);
         begin
            return Target'Base (X) * Target'Base (1.0 / 2 ** 53);
         end;
      else
         declare
            type Unsigned_1 is mod 2; -- high 1 bit
            type Unsigned_64 is mod 2 ** 64; -- low bits
            function Unsigned_1_Random is
               new Uniform_Discrete_Random (Source, Unsigned_1,
                  Generator, Get);
            function Unsigned_64_Random is
               new Uniform_Discrete_Random (Source, Unsigned_64,
                  Generator, Get);
         begin
            loop
               declare
                  X : constant Unsigned_64 := Unsigned_64_Random (Gen);
               begin
                  if Unsigned_1_Random (Gen) = 1 then
                     if X = 0 then -- a 128 bit random value = 2 ** 64
                        return 1.0;
                     end if;
                  else
                     return Target'Base (X) * Target'Base (1.0 / 2 ** 64);
                  end if;
               end;
            end loop;
         end;
      end if;
   end Uniform_Float_Random_0_To_1;

   function Uniform_Float_Random_0_To_Less_Than_1 (
      Gen : aliased in out Generator)
      return Target is
   begin
      if Target'Machine_Mantissa <= 24 then -- Float'Machine_Mantissa
         declare
            type Unsigned_24 is mod 2 ** 24;
            function Unsigned_24_Random is
               new Uniform_Discrete_Random (Source, Unsigned_24,
                  Generator, Get);
            function Float_0_To_Less_Than_1 is
               new Linear_Float_0_To_Less_Than_1 (Unsigned_24, Target);
         begin
            return Float_0_To_Less_Than_1 (Unsigned_24_Random (Gen));
         end;
      elsif Target'Machine_Mantissa <= 53 then -- Long_Float'Machine_Mantissa
         declare
            type Unsigned_53 is mod 2 ** 53;
            function Unsigned_53_Random is
               new Uniform_Discrete_Random (Source, Unsigned_53,
                  Generator, Get);
            function Float_0_To_Less_Than_1 is
               new Linear_Float_0_To_Less_Than_1 (Unsigned_53, Target);
         begin
            return Float_0_To_Less_Than_1 (Unsigned_53_Random (Gen));
         end;
      else
         declare
            type Unsigned_64 is mod 2 ** 64;
            function Unsigned_64_Random is
               new Uniform_Discrete_Random (Source, Unsigned_64,
                  Generator, Get);
            function Float_0_To_Less_Than_1 is
               new Linear_Float_0_To_Less_Than_1 (Unsigned_64, Target);
         begin
            return Float_0_To_Less_Than_1 (Unsigned_64_Random (Gen));
         end;
      end if;
   end Uniform_Float_Random_0_To_Less_Than_1;

   function Uniform_Float_Random_Greater_Than_0_To_Less_Than_1 (
      Gen : aliased in out Generator)
      return Target is
   begin
      if Target'Machine_Mantissa <= 24 then -- Float'Machine_Mantissa
         declare
            type Unsigned_24 is mod 2 ** 24;
            subtype Repr is Unsigned_24 range 1 .. Unsigned_24'Last;
            function Unsigned_24_Random is
               new Uniform_Discrete_Random (Source, Repr, Generator, Get);
            function Float_0_To_Less_Than_1 is
               new Linear_Float_0_To_Less_Than_1 (Unsigned_24, Target);
         begin
            return Float_0_To_Less_Than_1 (Unsigned_24_Random (Gen));
         end;
      elsif Target'Machine_Mantissa <= 53 then -- Long_Float'Machine_Mantissa
         declare
            type Unsigned_53 is mod 2 ** 53;
            subtype Repr is Unsigned_53 range 1 .. Unsigned_53'Last;
            function Unsigned_53_Random is
               new Uniform_Discrete_Random (Source, Repr, Generator, Get);
            function Float_0_To_Less_Than_1 is
               new Linear_Float_0_To_Less_Than_1 (Unsigned_53, Target);
         begin
            return Float_0_To_Less_Than_1 (Unsigned_53_Random (Gen));
         end;
      else
         declare
            type Unsigned_64 is mod 2 ** 64;
            subtype Repr is Unsigned_64 range 1 .. Unsigned_64'Last;
            function Unsigned_64_Random is
               new Uniform_Discrete_Random (Source, Repr, Generator, Get);
            function Float_0_To_Less_Than_1 is
               new Linear_Float_0_To_Less_Than_1 (Unsigned_64, Target);
         begin
            return Float_0_To_Less_Than_1 (Unsigned_64_Random (Gen));
         end;
      end if;
   end Uniform_Float_Random_Greater_Than_0_To_Less_Than_1;

end Ada.Numerics.Distributions;
