pragma Check_Policy (Validate, Off);
with Ada.Numerics.Initiator;
with Ada.Numerics.SFMT.Random.Inside;
with System.Formatting;
with System.Storage_Elements;
package body Ada.Numerics.SFMT.Random is
   pragma Suppress (All_Checks);
   use type Unsigned_32;
   use type Unsigned_64;
   use type System.Bit_Order;
   use type System.Storage_Elements.Storage_Count;

   package Impl is new Inside;

   function idxof (i : Integer) return Integer;
   pragma Inline (idxof);

   function func1 (x : Unsigned_32) return Unsigned_32;
   pragma Inline (func1);
   function func2 (x : Unsigned_32) return Unsigned_32;
   pragma Inline (func2);

   procedure period_certification (
      psfmt32 : in out Unsigned_32_Array_N32);

   --  This function simulate a 64-bit index of LITTLE ENDIAN
   --  in BIG ENDIAN machine.
   --  Note: This Ada version is always 32-bit.
   function idxof (i : Integer) return Integer
      renames "+";

   --  This function represents a function used in the initialization
   --  by init_by_array
   function func1 (x : Unsigned_32) return Unsigned_32 is
   begin
      return (x xor Interfaces.Shift_Right (x, 27)) * 1664525;
   end func1;

   --  This function represents a function used in the initialization
   --  by init_by_array
   function func2 (x : Unsigned_32) return Unsigned_32 is
   begin
      return (x xor Interfaces.Shift_Right (x, 27)) * 1566083941;
   end func2;

   --  This function certificate the period of 2^{MEXP}
   procedure period_certification (
      psfmt32 : in out Unsigned_32_Array_N32)
   is
      inner : Unsigned_32 := 0;
      work : Unsigned_32;
      parity : constant Unsigned_32_Array (0 .. 3) :=
         (PARITY1, PARITY2, PARITY3, PARITY4);
   begin
      for i in 0 .. 3 loop
         inner := inner xor (psfmt32 (idxof (i)) and parity (i));
      end loop;
      for i in reverse 0 .. 4 loop
         inner := inner xor Interfaces.Shift_Right (inner, 2 ** i);
      end loop;
      inner := inner and 1;
      --  check OK
      if inner = 1 then
         return;
      end if;
      --  check NG, and modification
      for i in 0 .. 3 loop
         work := 1;
         for j in 0 .. 31 loop
            if (work and parity (i)) /= 0 then
               declare
                  Index : constant Natural := idxof (i);
               begin
                  psfmt32 (Index) := psfmt32 (Index) xor work;
               end;
               return;
            end if;
            work := Interfaces.Shift_Left (work, 1);
         end loop;
      end loop;
   end period_certification;

   --  implementation

   --  This function returns the identification string.
   --  The string shows the word size, the Mersenne exponent,
   --  and all parameters of this generator.
   --  ex. "SFMT-19937:122-18-1-11-1:dfffffef-ddfecb7f-bffaffff-bffffff6"
   function Id return String is
      Result : String (
         1 ..
         4 + 1 -- "SFMT-"
            + 6 + 1 -- "216091:"
            + 3 + 1 + 2 + 1 + 1 + 1 + 2 + 1 + 1 + 1 -- "627-11-3-10-1:"
            + 8 + 1 + 8 + 1 + 8 + 1 + 8); -- "%.8x-%.8x-%.8x-%.8x"
      Last : Natural := 0;
      Error : Boolean;
   begin
      Result (Last + 1 .. Last + 5) := "SFMT-";
      Last := Last + 5;
      System.Formatting.Image (
         System.Formatting.Unsigned (MEXP),
         Result (Last + 1 .. Result'Last),
         Last,
         Error => Error);
      Result (Last + 1) := ':';
      Last := Last + 1;
      System.Formatting.Image (
         System.Formatting.Unsigned (POS1),
         Result (Last + 1 .. Result'Last),
         Last,
         Error => Error);
      Result (Last + 1) := '-';
      Last := Last + 1;
      System.Formatting.Image (
         System.Formatting.Unsigned (SL1),
         Result (Last + 1 .. Result'Last),
         Last,
         Error => Error);
      Result (Last + 1) := '-';
      Last := Last + 1;
      System.Formatting.Image (
         System.Formatting.Unsigned (SL2),
         Result (Last + 1 .. Result'Last),
         Last,
         Error => Error);
      Result (Last + 1) := '-';
      Last := Last + 1;
      System.Formatting.Image (
         System.Formatting.Unsigned (SR1),
         Result (Last + 1 .. Result'Last),
         Last,
         Error => Error);
      Result (Last + 1) := '-';
      Last := Last + 1;
      System.Formatting.Image (
         System.Formatting.Unsigned (SR2),
         Result (Last + 1 .. Result'Last),
         Last,
         Error => Error);
      Result (Last + 1) := ':';
      Last := Last + 1;
      System.Formatting.Image (
         System.Formatting.Unsigned (MSK1),
         Result (Last + 1 .. Result'Last),
         Last,
         Base => 16,
         Set => System.Formatting.Lower_Case,
         Width => 8,
         Error => Error);
      Result (Last + 1) := '-';
      Last := Last + 1;
      System.Formatting.Image (
         System.Formatting.Unsigned (MSK2),
         Result (Last + 1 .. Result'Last),
         Last,
         Base => 16,
         Set => System.Formatting.Lower_Case,
         Width => 8,
         Error => Error);
      Result (Last + 1) := '-';
      Last := Last + 1;
      System.Formatting.Image (
         System.Formatting.Unsigned (MSK3),
         Result (Last + 1 .. Result'Last),
         Last,
         Base => 16,
         Set => System.Formatting.Lower_Case,
         Width => 8,
         Error => Error);
      Result (Last + 1) := '-';
      Last := Last + 1;
      System.Formatting.Image (
         System.Formatting.Unsigned (MSK4),
         Result (Last + 1 .. Result'Last),
         Last,
         Base => 16,
         Set => System.Formatting.Lower_Case,
         Width => 8,
         Error => Error);
      return Result (1 .. Last);
   end Id;

   --  This function generates and returns 32-bit pseudorandom number.
   --  init_gen_rand or init_by_array must be called before this function.
   function Random_32 (Gen : aliased in out Generator)
      return Unsigned_32
   is
      psfmt32 : Unsigned_32_Array_N32;
      for psfmt32'Address use Gen.sfmt.state'Address;
      r : Unsigned_32;
   begin
      if Gen.sfmt.idx >= N32 then
         Impl.gen_rand_all (Gen.sfmt.state);
         Gen.sfmt.idx := 0;
      end if;
      r := psfmt32 (Gen.sfmt.idx);
      Gen.sfmt.idx := Gen.sfmt.idx + 1;
      return r;
   end Random_32;

   --  This function generates and returns 64-bit pseudorandom number.
   --  init_gen_rand or init_by_array must be called before this function.
   --  The function gen_rand64 should not be called after gen_rand32,
   --  unless an initialization is again executed.
   --  Note: This Ada version supports mixed callings with Random_32 (still).
   function Random_64 (Gen : aliased in out Generator)
      return Unsigned_64
   is
      psfmt32 : Unsigned_32_Array_N32;
      for psfmt32'Address use Gen.sfmt.state'Address;
      psfmt64 : Unsigned_64_Array_N64;
      for psfmt64'Address use Gen.sfmt.state'Address;
      r : Unsigned_64;
   begin
      if Gen.sfmt.idx rem 2 /= 0 then
         declare
            r1 : constant Unsigned_32 := Random_32 (Gen);
            r2 : constant Unsigned_32 := Random_32 (Gen);
         begin
            r := Interfaces.Shift_Left (Unsigned_64 (r2), 32)
               or Unsigned_64 (r1);
         end;
      else
         pragma Check (Validate, Gen.sfmt.idx rem 2 = 0);
         if Gen.sfmt.idx >= N32 then
            Impl.gen_rand_all (Gen.sfmt.state);
            Gen.sfmt.idx := 0;
         end if;
         if System.Default_Bit_Order /= System.Low_Order_First then
            declare
               r1 : constant Unsigned_32 := psfmt32 (Gen.sfmt.idx);
               r2 : constant Unsigned_32 := psfmt32 (Gen.sfmt.idx + 1);
            begin
               Gen.sfmt.idx := Gen.sfmt.idx + 2;
               r := Interfaces.Shift_Left (Unsigned_64 (r2), 32)
                  or Unsigned_64 (r1);
            end;
         else
            r := psfmt64 (Gen.sfmt.idx / 2);
            Gen.sfmt.idx := Gen.sfmt.idx + 2;
         end if;
      end if;
      return r;
   end Random_64;

   --  This function generates pseudorandom 32-bit integers in the
   --  specified array[] by one call. The number of pseudorandom integers
   --  is specified by the argument size, which must be at least 624 and a
   --  multiple of four.  The generation by this function is much faster
   --  than the following gen_rand function.
   procedure Fill_Random_32 (
      Gen : aliased in out Generator;
      Item : out Unsigned_32_Array)
   is
      size : constant Integer := Item'Length;
   begin
      if Gen.sfmt.idx /= N32
         or else size rem 4 /= 0
         or else size < N32
      then
         for I in Item'Range loop
            Item (I) := Random_32 (Gen);
         end loop;
      else
         declare
            the_array : w128_t_Array_Fixed;
            for the_array'Address use Item'Address;
         begin
            Impl.gen_rand_array (Gen.sfmt.state, the_array, size / 4);
         end;
         Gen.sfmt.idx := N32;
      end if;
   end Fill_Random_32;

   --  This function generates pseudorandom 64-bit integers in the
   --  specified array[] by one call. The number of pseudorandom integers
   --  is specified by the argument size, which must be at least 312 and a
   --  multiple of two.  The generation by this function is much faster
   --  than the following gen_rand function.
   procedure Fill_Random_64 (
      Gen : aliased in out Generator;
      Item : out Unsigned_64_Array)
   is
      size : constant Integer := Item'Length;
   begin
      if Gen.sfmt.idx /= N32
         or else size rem 2 /= 0
         or else size < N64
      then
         for I in Item'Range loop
            Item (I) := Random_64 (Gen);
         end loop;
      else
         declare
            the_array : w128_t_Array_Fixed;
            for the_array'Address use Item'Address;
         begin
            Impl.gen_rand_array (Gen.sfmt.state, the_array, size / 2);
            if System.Default_Bit_Order /= System.Low_Order_First then
               --  swap
               for I in 0 .. size / 2 - 1 loop
                  declare
                     a : w128_t renames the_array (I);
                     x : constant Unsigned_32 := a (0);
                     y : constant Unsigned_32 := a (2);
                  begin
                     a (0) := a (1);
                     a (2) := a (3);
                     a (1) := x;
                     a (3) := y;
                  end;
               end loop;
            end if;
         end;
         Gen.sfmt.idx := N32;
      end if;
   end Fill_Random_64;

   function Initialize return Generator is
   begin
      return (sfmt => Initialize);
   end Initialize;

   function Initialize (Initiator : Unsigned_32) return Generator is
   begin
      return (sfmt => Initialize (Initiator));
   end Initialize;

   function Initialize (Initiator : Unsigned_32_Array) return Generator is
   begin
      return (sfmt => Initialize (Initiator));
   end Initialize;

   procedure Reset (Gen : in out Generator) is
   begin
      Gen.sfmt := Initialize;
   end Reset;

   procedure Reset (Gen : in out Generator; Initiator : Integer) is
   begin
      Gen.sfmt := Initialize (Unsigned_32'Mod (Initiator));
   end Reset;

   function Reset (From_State : State) return Generator is
   begin
      return (sfmt => From_State);
   end Reset;

   function Initialize return State is
      Init : Unsigned_32_Array (0 .. N32 - 1);
   begin
      Initiator (Init'Address, Init'Size / Standard'Storage_Unit);
      return Initialize (Init);
   end Initialize;

   --  This function initializes the internal state array with a 32-bit
   --  integer seed.
   function Initialize (Initiator : Unsigned_32) return State is
   begin
      return Result : State := (
         state => <>,
         idx => N32)
      do
         declare
            psfmt32 : Unsigned_32_Array_N32;
            for psfmt32'Address use Result.state'Address;
         begin
            psfmt32 (idxof (0)) := Initiator;
            for i in 1 .. N32 - 1 loop
               psfmt32 (idxof (i)) :=
                  1812433253
                     * (
                        psfmt32 (idxof (i - 1))
                        xor (
                           Interfaces.Shift_Right (
                              psfmt32 (idxof (i - 1)),
                              30)))
                  + Unsigned_32 (i);
            end loop;
            period_certification (psfmt32);
         end;
      end return;
   end Initialize;

   --  This function initializes the internal state array,
   --  with an array of 32-bit integers used as the seeds
   function Initialize (Initiator : Unsigned_32_Array) return State is
      key_length : constant Natural := Initiator'Length;
      i, j, count : Integer;
      r : Unsigned_32;
      lag : Integer;
      mid : Integer;
      size : constant Natural := N * 4;
   begin
      if size >= 623 then
         lag := 11;
      elsif size >= 68 then
         lag := 7;
      elsif size >= 39 then
         lag := 5;
      else
         lag := 3;
      end if;
      mid := (size - lag) / 2;
      return Result : State := (
         state => (others => (others => 16#8b8b8b8b#)),
         idx => N32)
      do
         declare
            psfmt32 : Unsigned_32_Array_N32;
            for psfmt32'Address use Result.state'Address;
         begin
            if key_length + 1 > N32 then
               count := key_length + 1;
            else
               count := N32;
            end if;
            r := func1 (
               psfmt32 (idxof (0))
               xor psfmt32 (idxof (mid))
               xor psfmt32 (idxof (N32 - 1)));
            declare
               Index : constant Natural := idxof (mid);
            begin
               psfmt32 (Index) := psfmt32 (Index) + r;
            end;
            r := r + Unsigned_32 (key_length);
            declare
               Index : constant Natural := idxof (mid + lag);
            begin
               psfmt32 (Index) := psfmt32 (Index) + r;
            end;
            psfmt32 (idxof (0)) := r;
            count := count - 1;
            i := 1;
            j := 0;
            while j < count and then j < key_length loop
               r := func1 (
                  psfmt32 (idxof (i))
                  xor psfmt32 (idxof ((i + mid) rem N32))
                  xor psfmt32 (idxof ((i + N32 - 1) rem N32)));
               declare
                  Index : constant Natural := idxof ((i + mid) rem N32);
               begin
                  psfmt32 (Index) := psfmt32 (Index) + r;
               end;
               r := r + Initiator (Initiator'First + j) + Unsigned_32 (i);
               declare
                  Index : constant Natural := idxof ((i + mid + lag) rem N32);
               begin
                  psfmt32 (Index) := psfmt32 (Index) + r;
               end;
               psfmt32 (idxof (i)) := r;
               i := (i + 1) rem N32;
               j := j + 1;
            end loop;
            while j < count loop
               r := func1 (
                  psfmt32 (idxof (i))
                  xor psfmt32 (idxof ((i + mid) rem N32))
                  xor psfmt32 (idxof ((i + N32 - 1) rem N32)));
               declare
                  Index : constant Natural := idxof ((i + mid) rem N32);
               begin
                  psfmt32 (Index) := psfmt32 (Index) + r;
               end;
               r := r + Unsigned_32 (i);
               declare
                  Index : constant Natural := idxof ((i + mid + lag) rem N32);
               begin
                  psfmt32 (Index) := psfmt32 (Index) + r;
               end;
               psfmt32 (idxof (i)) := r;
               i := (i + 1) rem N32;
               j := j + 1;
            end loop;
            j := 0;
            while j < N32 loop
               r := func2 (
                  psfmt32 (idxof (i))
                  + psfmt32 (idxof ((i + mid) rem N32))
                  + psfmt32 (idxof ((i + N32 - 1) rem N32)));
               declare
                  Index : constant Natural := idxof ((i + mid) rem N32);
               begin
                  psfmt32 (Index) := psfmt32 (Index) xor r;
               end;
               r := r - Unsigned_32 (i);
               declare
                  Index : constant Natural := idxof ((i + mid + lag) rem N32);
               begin
                  psfmt32 (Index) := psfmt32 (Index) xor r;
               end;
               psfmt32 (idxof (i)) := r;
               i := (i + 1) rem N32;
               j := j + 1;
            end loop;
            period_certification (psfmt32);
         end;
      end return;
   end Initialize;

   procedure Save (Gen : Generator; To_State : out State) is
   begin
      To_State := Gen.sfmt;
   end Save;

   procedure Reset (Gen : in out Generator; From_State : State) is
   begin
      Gen.sfmt := From_State;
   end Reset;

   function Image (Of_State : State) return String is
      procedure Hex_Put (To : out String; Item : Unsigned_32);
      procedure Hex_Put (To : out String; Item : Unsigned_32) is
         Error : Boolean;
         Last : Natural;
      begin
         pragma Compile_Time_Error (
            System.Formatting.Unsigned'Size < 32,
            "integer size < 32");
         System.Formatting.Image (
            System.Formatting.Unsigned (Item),
            To,
            Last,
            Base => 16,
            Width => 32 / 4,
            Error => Error);
         pragma Check (Validate, not Error and then Last = To'Last);
      end Hex_Put;
      psfmt32 : Unsigned_32_Array_N32;
      for psfmt32'Address use Of_State.state'Address;
      Last : Natural := 0;
   begin
      return Result : String (1 .. Max_Image_Width) do
         for I in 0 .. N32 - 1 loop
            declare
               Previous_Last : constant Natural := Last;
            begin
               Last := Last + 32 / 4;
               Hex_Put (Result (Previous_Last + 1 .. Last), psfmt32 (I));
               Last := Last + 1;
               Result (Last) := ':';
            end;
         end loop;
         Hex_Put (
            Result (Last + 1 .. Result'Last),
            Unsigned_32 (Of_State.idx));
      end return;
   end Image;

   function Value (Coded_State : String) return State is
      procedure Hex_Get (From : String; Item : out Unsigned_32);
      procedure Hex_Get (From : String; Item : out Unsigned_32) is
         Last : Positive;
         Result : System.Formatting.Unsigned;
         Error : Boolean;
      begin
         System.Formatting.Value (
            From,
            Last,
            Result,
            Base => 16,
            Error => Error);
         if Error or else Last /= From'Last then
            raise Constraint_Error;
         end if;
         Item := Unsigned_32 (Result);
      end Hex_Get;
      Last : Natural := Coded_State'First - 1;
      idx : Unsigned_32;
   begin
      if Coded_State'Length /= Max_Image_Width then
         raise Constraint_Error;
      end if;
      return Result : State do
         declare
            psfmt32 : Unsigned_32_Array_N32;
            for psfmt32'Address use Result.state'Address;
         begin
            for I in 0 .. N32 - 1 loop
               declare
                  Previous_Last : constant Natural := Last;
               begin
                  Last := Last + 32 / 4;
                  Hex_Get (
                     Coded_State (Previous_Last + 1 .. Last),
                     psfmt32 (I));
                  Last := Last + 1;
                  if Coded_State (Last) /= ':' then
                     raise Constraint_Error;
                  end if;
               end;
            end loop;
         end;
         Hex_Get (Coded_State (Last + 1 .. Coded_State'Last), idx);
         Result.idx := Integer (idx);
      end return;
   end Value;

   --  The following real versions are due to Isaku Wada

   --  converts an unsigned 32-bit number to a double on [0,1]-real-interval.
   function To_0_To_1 (v : Unsigned_32)
      return Uniformly_Distributed is
   begin
      return Long_Long_Float (v) * (1.0 / (2.0 ** 32 - 1.0));
      --  divided by 2^32-1
   end To_0_To_1;

   --  generates a random number on [0,1]-real-interval
   function Random_0_To_1 (Gen : aliased in out Generator)
      return Uniformly_Distributed is
   begin
      return To_0_To_1 (Random_32 (Gen));
   end Random_0_To_1;

   --  converts an unsigned 32-bit integer to a double on [0,1)-real-interval.
   function To_0_To_Less_Than_1 (v : Unsigned_32)
      return Uniformly_Distributed is
   begin
      return Long_Long_Float (v) * (1.0 / 2.0 ** 32);
      --  divided by 2^32
   end To_0_To_Less_Than_1;

   --  generates a random number on [0,1)-real-interval
   function Random_0_To_Less_Than_1 (Gen : aliased in out Generator)
      return Uniformly_Distributed is
   begin
      return To_0_To_Less_Than_1 (Random_32 (Gen));
   end Random_0_To_Less_Than_1;

   --  converts an unsigned 32-bit integer to a double on (0,1)-real-interval.
   function To_Greater_Than_0_To_Less_Than_1 (v : Unsigned_32)
      return Uniformly_Distributed is
   begin
      return (Long_Long_Float (v) + 0.5) * (1.0 / 2.0 ** 32);
      --  divided by 2^32
   end To_Greater_Than_0_To_Less_Than_1;

   --  generates a random number on (0,1)-real-interval
   function Random_Greater_Than_0_To_Less_Than_1 (
      Gen : aliased in out Generator)
      return Uniformly_Distributed is
   begin
      return To_Greater_Than_0_To_Less_Than_1 (Random_32 (Gen));
   end Random_Greater_Than_0_To_Less_Than_1;

   --  converts an unsigned 32-bit integer to double on [0,1)
   --  with 53-bit resolution.
   --  Note: This Ada version is implemented with extended float,
   --        it has 64-bit resolution on x86 (32bit mode).
   function To_53_0_To_Less_Than_1 (v : Unsigned_64)
      return Uniformly_Distributed is
   begin
      return Long_Long_Float (v) * (1.0 / 2.0 ** 64);
   end To_53_0_To_Less_Than_1;

   --  generates a random number on [0,1) with 53-bit resolution
   function Random_53_0_To_Less_Than_1 (Gen : aliased in out Generator)
      return Uniformly_Distributed is
   begin
      return To_53_0_To_Less_Than_1 (Random_64 (Gen));
   end Random_53_0_To_Less_Than_1;

   --  Note: to_res53_mix and genrand_res53_mix are unimplemented.

end Ada.Numerics.SFMT.Random;
