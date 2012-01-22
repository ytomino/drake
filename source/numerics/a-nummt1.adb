with Ada.Numerics.Initiator;
with System.Storage_Elements;
with System.Formatting;
package body Ada.Numerics.MT19937 is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Count;
   use type Cardinal;

   MATRIX_A : constant Cardinal := 16#9908b0df#;
   UPPER_MASK : constant Cardinal := 2 ** (Cardinal'Size - 1);
   LOWER_MASK : constant Cardinal := not UPPER_MASK;

   --  implementation

   function Random_32 (Gen : not null access Generator) return Cardinal is
      mag01 : constant array (Cardinal range 0 .. 1) of Cardinal :=
         (0, MATRIX_A);
      y : Cardinal;
      S : State
         renames Gen.State;
   begin
      if S.Condition >= N then
         for kk in 0 .. (N - M - 1) loop
            y := (S.Vector (kk) and UPPER_MASK) or
               (S.Vector (kk + 1) and LOWER_MASK);
            S.Vector (kk) := S.Vector (kk + M) xor
               Interfaces.Shift_Right (y, 1) xor mag01 (y and 1);
         end loop;
         for kk in (N - M) .. (N - 2) loop
            y := (S.Vector (kk) and UPPER_MASK) or
               (S.Vector (kk + 1) and LOWER_MASK);
            S.Vector (kk) := S.Vector (kk + (M - N)) xor
               Interfaces.Shift_Right (y, 1) xor mag01 (y and 1);
         end loop;
         y := (S.Vector (N - 1) and UPPER_MASK) or
            (S.Vector (0) and LOWER_MASK);
         S.Vector (N - 1) := S.Vector (M - 1) xor
            Interfaces.Shift_Right (y, 1) xor mag01 (y and 1);
         S.Condition := 0;
      end if;
      y := S.Vector (Integer (S.Condition));
      S.Condition := S.Condition + 1;
      y := y xor Interfaces.Shift_Right (y, 11);
      y := y xor (Interfaces.Shift_Left (y, 7) and 16#9d2c5680#);
      y := y xor (Interfaces.Shift_Left (y, 15) and 16#efc60000#);
      y := y xor Interfaces.Shift_Right (y, 18);
      return y;
   end Random_32;

   function Initialize return Generator is
   begin
      return (State => Initialize);
   end Initialize;

   function Initialize (Initiator : Cardinal) return Generator is
   begin
      return (State => Initialize (Initiator));
   end Initialize;

   function Initialize (Initiator : Cardinal_Vector) return Generator is
   begin
      return (State => Initialize (Initiator));
   end Initialize;

   procedure Reset (Gen : in out Generator) is
   begin
      Gen.State := Initialize;
   end Reset;

   procedure Reset (Gen : in out Generator; Initiator : Integer) is
   begin
      Gen.State := Initialize (Cardinal'Mod (Initiator));
   end Reset;

   function Initialize return State is
      Init : Cardinal_Vector (N_Range);
   begin
      Initiator (Init'Address, Init'Size / Standard'Storage_Unit);
      return Initialize (Init);
   end Initialize;

   function Initialize (Initiator : Cardinal) return State is
      V : Cardinal := Initiator;
   begin
      return S : State do
         S.Vector (0) := V;
         for I in 1 .. (N - 1) loop
            V := 1812433253 * (V xor Interfaces.Shift_Right (V, 30)) +
               Cardinal (I);
            S.Vector (I) := V;
         end loop;
         S.Condition := N;
      end return;
   end Initialize;

   function Initialize (Initiator : Cardinal_Vector) return State is
      i : Integer := 1;
      j : Integer := 0;
   begin
      return S : State := Initialize (19650218) do
         for K in reverse 1 .. Cardinal'Max (N, Initiator'Length) loop
            declare
               P : constant Cardinal := S.Vector (i - 1);
            begin
               S.Vector (i) := (S.Vector (i) xor
                  ((P xor Interfaces.Shift_Right (P, 30)) * 1664525)) +
                  Initiator (Initiator'First + j) + Cardinal (j);
            end;
            i := i + 1;
            if i >= N then
               S.Vector (0) := S.Vector (N - 1);
               i := 1;
            end if;
            j := (j + 1) rem Initiator'Length;
         end loop;
         for K in reverse 1 .. (N - 1) loop
            declare
               P : constant Cardinal := S.Vector (i - 1);
            begin
               S.Vector (i) := (S.Vector (i) xor
                  ((P xor Interfaces.Shift_Right (P, 30)) * 1566083941))
                  - Cardinal (i);
            end;
            i := i + 1;
            if i >= N then
               S.Vector (0) := S.Vector (N - 1);
               i := 1;
            end if;
         end loop;
         S.Vector (0) := 16#80000000#;
      end return;
   end Initialize;

   procedure Save (Gen : Generator; To_State : out State) is
   begin
      To_State := Gen.State;
   end Save;

   procedure Reset (Gen : in out Generator; From_State : State) is
   begin
      Gen.State := From_State;
   end Reset;

   function Reset (From_State : State) return Generator is
   begin
      return (State => From_State);
   end Reset;

   function Image (Of_State : State) return String is
      procedure Hex (Result : in out String; Value : Cardinal);
      procedure Hex (Result : in out String; Value : Cardinal) is
         Error : Boolean;
         Last : Natural;
      begin
         pragma Compile_Time_Error (
            System.Formatting.Unsigned'Size < Cardinal'Size,
            "integer size < 32");
         System.Formatting.Image (
            System.Formatting.Unsigned (Value),
            Result,
            Last,
            Base => 16,
            Width => Cardinal'Size / 4,
            Error => Error);
         pragma Assert (not Error and then Last = Result'Last);
      end Hex;
      Result : String (1 .. Max_Image_Width);
      Position : Positive := Result'First;
   begin
      for I in N_Range loop
         Hex (
            Result (Position .. Position + Cardinal'Size / 4 - 1),
            Of_State.Vector (I));
         Position := Position + Cardinal'Size / 4;
         Result (Position) := ':';
         Position := Position + 1;
      end loop;
      Hex (Result (Position .. Result'Last), Of_State.Condition);
      return Result;
   end Image;

   function Value (Coded_State : String) return State is
      procedure Hex (Item : String; Value : out Cardinal);
      procedure Hex (Item : String; Value : out Cardinal) is
         Last : Positive;
         Result : System.Formatting.Unsigned;
         Error : Boolean;
      begin
         System.Formatting.Value (
            Item,
            Last,
            Result,
            Base => 16,
            Error => Error);
         if Error or else Last /= Item'Last then
            raise Constraint_Error;
         end if;
         Value := Cardinal (Result);
      end Hex;
      Result : State;
      Position : Positive := Coded_State'First;
   begin
      if Coded_State'Length /= Max_Image_Width then
         raise Constraint_Error;
      end if;
      for I in N_Range loop
         Hex (
            Coded_State (Position .. Position + Cardinal'Size / 4 - 1),
            Result.Vector (I));
         Position := Position + Cardinal'Size / 4;
         if Coded_State (Position) /= ':' then
            raise Constraint_Error;
         end if;
         Position := Position + 1;
      end loop;
      Hex (Coded_State (Position .. Coded_State'Last), Result.Condition);
      return Result;
   end Value;

   function Random_0_To_1 (Gen : not null access Generator)
      return Uniformly_Distributed is
   begin
      return Uniformly_Distributed'Base (Random_32 (Gen))
         * (1.0 / 4294967295.0);
   end Random_0_To_1;

   function Random_0_To_Less_1 (Gen : not null access Generator)
      return Uniformly_Distributed is
   begin
      return Uniformly_Distributed'Base (Random_32 (Gen)) *
         (1.0 / 4294967296.0);
   end Random_0_To_Less_1;

   function Random_Greater_0_To_Less_1 (Gen : not null access Generator)
      return Uniformly_Distributed is
   begin
      return (Uniformly_Distributed'Base (Random_32 (Gen)) + 0.5) *
         (1.0 / 4294967296.0);
   end Random_Greater_0_To_Less_1;

   function Random_53_0_To_Less_1 (Gen : not null access Generator)
      return Uniformly_Distributed
   is
      A : constant Cardinal := Interfaces.Shift_Right (Random_32 (Gen), 5);
      B : constant Cardinal := Interfaces.Shift_Right (Random_32 (Gen), 6);
      Float_A : constant Long_Long_Float := Uniformly_Distributed'Base (A);
      Float_B : constant Long_Long_Float := Uniformly_Distributed'Base (B);
   begin
      return (Float_A * 67108864.0 + Float_B) * (1.0 / 9007199254740992.0);
   end Random_53_0_To_Less_1;

   package body Discrete_Random is
      pragma Suppress (All_Checks);

      function Random (Gen : not null access Generator)
         return Result_Subtype
      is
         subtype Real is Long_Long_Float;
         Random : constant Real :=
            MT19937.Random_0_To_Less_1 (Gen);
         Position : constant Integer :=
            Result_Subtype'Pos (Result_Subtype'First) +
            Integer (Real'Floor (Random * Real (Result_Subtype'Range_Length)));
      begin
         return Result_Subtype'Val (Position);
      end Random;

   end Discrete_Random;

end Ada.Numerics.MT19937;
