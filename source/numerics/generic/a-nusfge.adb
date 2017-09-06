package body Ada.Numerics.SFMT.Generating is
   --  no SIMD version
   use type Unsigned_32;
   use type Unsigned_64;

   procedure rshift128 (
      Out_Item : out w128_t;
      In_Item : w128_t;
      shift : Integer)
      with Convention => Intrinsic;
   procedure lshift128 (
      Out_Item : out w128_t;
      In_Item : w128_t;
      shift : Integer)
      with Convention => Intrinsic;

   pragma Inline_Always (rshift128);
   pragma Inline_Always (lshift128);

   procedure do_recursion (r : out w128_t; a, b, c, d : w128_t)
      with Convention => Intrinsic;
   pragma Inline_Always (do_recursion);

   --  This function simulates SIMD 128-bit right shift by the standard C.
   --  The 128-bit integer given in in is shifted by (shift * 8) bits.
   --  This function simulates the LITTLE ENDIAN SIMD.
   procedure rshift128 (
      Out_Item : out w128_t;
      In_Item : w128_t;
      shift : Integer)
   is
      th, tl, oh, ol : Unsigned_64;
   begin
      th := Interfaces.Shift_Left (Unsigned_64 (In_Item (3)), 32)
         or Unsigned_64 (In_Item (2));
      tl := Interfaces.Shift_Left (Unsigned_64 (In_Item (1)), 32)
         or Unsigned_64 (In_Item (0));
      oh := Interfaces.Shift_Right (th, shift * 8);
      ol := Interfaces.Shift_Right (tl, shift * 8);
      ol := ol or Interfaces.Shift_Left (th, 64 - shift * 8);
      Out_Item (1) := Unsigned_32'Mod (Interfaces.Shift_Right (ol, 32));
      Out_Item (0) := Unsigned_32'Mod (ol);
      Out_Item (3) := Unsigned_32'Mod (Interfaces.Shift_Right (oh, 32));
      Out_Item (2) := Unsigned_32'Mod (oh);
   end rshift128;

   --  This function simulates SIMD 128-bit left shift by the standard C.
   --  The 128-bit integer given in in is shifted by (shift * 8) bits.
   --  This function simulates the LITTLE ENDIAN SIMD.
   procedure lshift128 (
      Out_Item : out w128_t;
      In_Item : w128_t;
      shift : Integer)
   is
      th, tl, oh, ol : Unsigned_64;
   begin
      th := Interfaces.Shift_Left (Unsigned_64 (In_Item (3)), 32)
         or Unsigned_64 (In_Item (2));
      tl := Interfaces.Shift_Left (Unsigned_64 (In_Item (1)), 32)
         or Unsigned_64 (In_Item (0));
      oh := Interfaces.Shift_Left (th, shift * 8);
      ol := Interfaces.Shift_Left (tl, shift * 8);
      oh := oh or Interfaces.Shift_Right (tl, 64 - shift * 8);
      Out_Item (1) := Unsigned_32'Mod (Interfaces.Shift_Right (ol, 32));
      Out_Item (0) := Unsigned_32'Mod (ol);
      Out_Item (3) := Unsigned_32'Mod (Interfaces.Shift_Right (oh, 32));
      Out_Item (2) := Unsigned_32'Mod (oh);
   end lshift128;

   --  This function represents the recursion formula.
   procedure do_recursion (r : out w128_t; a, b, c, d : w128_t) is
      x : w128_t;
      y : w128_t;
   begin
      lshift128 (x, a, SL2);
      rshift128 (y, c, SR2);
      r (0) := a (0) xor x (0)
         xor (Interfaces.Shift_Right (b (0), SR1) and MSK1) xor y (0)
         xor Interfaces.Shift_Left (d (0), SL1);
      r (1) := a (1) xor x (1)
         xor (Interfaces.Shift_Right (b (1), SR1) and MSK2) xor y (1)
         xor Interfaces.Shift_Left (d (1), SL1);
      r (2) := a (2) xor x (2)
         xor (Interfaces.Shift_Right (b (2), SR1) and MSK3) xor y (2)
         xor Interfaces.Shift_Left (d (2), SL1);
      r (3) := a (3) xor x (3)
         xor (Interfaces.Shift_Right (b (3), SR1) and MSK4) xor y (3)
         xor Interfaces.Shift_Left (d (3), SL1);
   end do_recursion;

   --  implementation

   --  This function fills the internal state array with pseudorandom
   --  integers.
   procedure gen_rand_all (
      sfmt : in out w128_t_Array_N)
   is
      i : Integer;
      r1, r2 : access w128_t;
   begin
      r1 := sfmt (N - 2)'Access;
      r2 := sfmt (N - 1)'Access;
      i := 0;
      while i < N - POS1 loop
         do_recursion (
            sfmt (i),
            sfmt (i),
            sfmt (i + POS1),
            r1.all,
            r2.all);
         r1 := r2;
         r2 := sfmt (i)'Access;
         i := i + 1;
      end loop;
      while i < N loop
         do_recursion (
            sfmt (i),
            sfmt (i),
            sfmt (i + POS1 - N),
            r1.all,
            r2.all);
         r1 := r2;
         r2 := sfmt (i)'Access;
         i := i + 1;
      end loop;
   end gen_rand_all;

   --  This function fills the user-specified array with pseudorandom
   --  integers.
   procedure gen_rand_array (
      sfmt : in out w128_t_Array_N;
      Item : aliased in out w128_t;
      size : Integer)
   is
      pragma Suppress (Alignment_Check);
      the_array : w128_t_Array (0 .. size - 1);
      for the_array'Address use Item'Address;
      i, j : Integer;
      r1, r2 : access w128_t;
   begin
      r1 := sfmt (N - 2)'Access;
      r2 := sfmt (N - 1)'Access;
      i := 0;
      while i < N - POS1 loop
         do_recursion (
            the_array (i),
            sfmt (i),
            sfmt (i + POS1),
            r1.all,
            r2.all);
         r1 := r2;
         r2 := the_array (i)'Access;
         i := i + 1;
      end loop;
      while i < N loop
         do_recursion (
            the_array (i),
            sfmt (i),
            the_array (i + POS1 - N),
            r1.all,
            r2.all);
         r1 := r2;
         r2 := the_array (i)'Access;
         i := i + 1;
      end loop;
      while i < size - N loop
         do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i + POS1 - N),
            r1.all,
            r2.all);
         r1 := r2;
         r2 := the_array (i)'Access;
         i := i + 1;
      end loop;
      j := 0;
      while j < 2 * N - size loop
         sfmt (j) := the_array (j + size - N);
         j := j + 1;
      end loop;
      while i < size loop
         do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i + POS1 - N),
            r1.all,
            r2.all);
         r1 := r2;
         r2 := the_array (i)'Access;
         sfmt (j) := the_array (i);
         i := i + 1;
         j := j + 1;
      end loop;
   end gen_rand_array;

end Ada.Numerics.SFMT.Generating;
