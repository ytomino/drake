with Ada.Unchecked_Conversion;
package body Ada.Numerics.SFMT.Generating is
   --  SSE2 version

   type v4si is array (1 .. 4) of Unsigned_32;
   for v4si'Alignment use 16;
   pragma Machine_Attribute (v4si, "vector_type");
   pragma Machine_Attribute (v4si, "may_alias");
   pragma Suppress_Initialization (v4si);

   type m128i is array (1 .. 2) of Unsigned_64;
   for m128i'Alignment use 16;
   pragma Machine_Attribute (m128i, "vector_type");
   pragma Machine_Attribute (m128i, "may_alias");
   pragma Suppress_Initialization (m128i);

   function To_v4si is new Unchecked_Conversion (m128i, v4si);
   function To_m128i is new Unchecked_Conversion (v4si, m128i);

   function ia32_psrldi128 (A : v4si; B : Integer) return v4si
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_psrldi128";

   function ia32_pslldi128 (A : v4si; B : Integer) return v4si
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_pslldi128";

   function mm_srli_si128 (A : m128i; B : Integer) return m128i
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_psrldqi128";

   function mm_slli_si128 (A : m128i; B : Integer) return m128i
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_pslldqi128";

   function mm_and_si128 (A, B : m128i) return m128i
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_pand128";

   function mm_xor_si128 (A, B : m128i) return m128i
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_pxor128";

   procedure mm_recursion (r : out m128i; a, b, c, d : m128i)
      with Convention => Intrinsic;

   pragma Inline_Always (mm_recursion);

   --  This function represents the recursion formula.
   procedure mm_recursion (r : out m128i; a, b, c, d : m128i) is
      --  parameters used by sse2.
      sse2_param_mask : constant v4si := (MSK1, MSK2, MSK3, MSK4);
      v, x, y, z : m128i;
   begin
      y := To_m128i (ia32_psrldi128 (To_v4si (b), SR1)); -- mm_srli_epi32
      z := mm_srli_si128 (c, SR2 * 8);
      v := To_m128i (ia32_pslldi128 (To_v4si (d), SL1)); -- mm_slli_epi32
      z := mm_xor_si128 (z, a);
      z := mm_xor_si128 (z, v);
      x := mm_slli_si128 (a, SL2 * 8);
      y := mm_and_si128 (y, To_m128i (sse2_param_mask));
      z := mm_xor_si128 (z, x);
      z := mm_xor_si128 (z, y);
      r := z;
   end mm_recursion;

   --  implementation

   --  This function fills the internal state array with pseudorandom
   --  integers.
   procedure gen_rand_all (
      sfmt : in out w128_t_Array_N)
   is
      i : Integer;
      r1, r2 : m128i;
      pstate_si : array (0 .. sfmt'Size / 128 - 1) of aliased m128i;
      for pstate_si'Address use sfmt'Address;
   begin
      r1 := pstate_si (N - 2);
      r2 := pstate_si (N - 1);
      i := 0;
      while i < N - POS1 loop
         mm_recursion (
            pstate_si (i),
            pstate_si (i),
            pstate_si (i + POS1),
            r1,
            r2);
         r1 := r2;
         r2 := pstate_si (i);
         i := i + 1;
      end loop;
      while i < N loop
         mm_recursion (
            pstate_si (i),
            pstate_si (i),
            pstate_si (i + POS1 - N),
            r1,
            r2);
         r1 := r2;
         r2 := pstate_si (i);
         i := i + 1;
      end loop;
   end gen_rand_all;

   --  This function fills the user-specified array with pseudorandom
   --  integers.
   procedure gen_rand_array (
      sfmt : in out w128_t_Array_N;
      Item : in out w128_t_Array_Fixed;
      size : Integer)
   is
      i, j : Integer;
      r1, r2 : m128i;
      pstate_si : array (0 .. sfmt'Size / 128 - 1) of aliased m128i;
      for pstate_si'Address use sfmt'Address;
      array_si : array (Natural) of aliased m128i;
      for array_si'Address use Item'Address;
   begin
      r1 := pstate_si (N - 2);
      r2 := pstate_si (N - 1);
      i := 0;
      while i < N - POS1 loop
         mm_recursion (
            array_si (i),
            pstate_si (i),
            pstate_si (i + POS1),
            r1,
            r2);
         r1 := r2;
         r2 := array_si (i);
         i := i + 1;
      end loop;
      while i < N loop
         mm_recursion (
            array_si (i),
            pstate_si (i),
            array_si (i + POS1 - N),
            r1,
            r2);
         r1 := r2;
         r2 := array_si (i);
         i := i + 1;
      end loop;
      --  main loop
      while i < size - N loop
         mm_recursion (
            array_si (i),
            array_si (i - N),
            array_si (i + POS1 - N),
            r1,
            r2);
         r1 := r2;
         r2 := array_si (i);
         i := i + 1;
      end loop;
      j := 0;
      while j < 2 * N - size loop
         pstate_si (j) := array_si (j + size - N);
         j := j + 1;
      end loop;
      while i < size loop
         mm_recursion (
            array_si (i),
            array_si (i - N),
            array_si (i + POS1 - N),
            r1,
            r2);
         r1 := r2;
         r2 := array_si (i);
         pstate_si (j) := array_si (i);
         i := i + 1;
         j := j + 1;
      end loop;
   end gen_rand_array;

end Ada.Numerics.SFMT.Generating;
