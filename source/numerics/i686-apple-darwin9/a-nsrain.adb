with Ada.Unchecked_Conversion;
package body Ada.Numerics.SFMT.Random.Inside is
   --  SSE2 version
   pragma Suppress (All_Checks);

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

   function mm_set_epi32 (q3, q2, q1, q0 : Unsigned_32) return m128i;
   pragma Inline_Always (mm_set_epi32);
--  pragma Machine_Attribute (mm_set_epi32, "artificial"); -- ignored?
   function mm_set_epi32 (q3, q2, q1, q0 : Unsigned_32) return m128i is
   begin
      return To_m128i (v4si'(q0, q1, q2, q3));
   end mm_set_epi32;

--  function mm_load_si128 (P : access constant m128i) return m128i;
--  pragma Inline_Always (mm_load_si128);
--  pragma Machine_Attribute (mm_load_si128, "artificial"); -- ignored?
--  function mm_load_si128 (P : access constant m128i) return m128i is
--  begin
--    return P.all;
--  end mm_load_si128;

--  procedure mm_store_si128 (P : access m128i; B : m128i);
--  pragma Inline_Always (mm_store_si128);
--  pragma Machine_Attribute (mm_store_si128, "artificial"); -- ignored?
--  procedure mm_store_si128 (P : access m128i; B : m128i) is
--  begin
--    P.all := B;
--  end mm_store_si128;

   function ia32_psrldi128 (A : v4si; B : Integer) return v4si;
   pragma Import (Intrinsic, ia32_psrldi128, "__builtin_ia32_psrldi128");

   function ia32_pslldi128 (A : v4si; B : Integer) return v4si;
   pragma Import (Intrinsic, ia32_pslldi128, "__builtin_ia32_pslldi128");

   function mm_srli_si128 (A : m128i; B : Integer) return m128i;
   pragma Import (Intrinsic, mm_srli_si128, "__builtin_ia32_psrldqi128");

   function mm_slli_si128 (A : m128i; B : Integer) return m128i;
   pragma Import (Intrinsic, mm_slli_si128, "__builtin_ia32_pslldqi128");

   function mm_and_si128 (A, B : m128i) return m128i;
   pragma Import (Intrinsic, mm_and_si128, "__builtin_ia32_pand128");

   function mm_xor_si128 (A, B : m128i) return m128i;
   pragma Import (Intrinsic, mm_xor_si128, "__builtin_ia32_pxor128");

   function mm_recursion (a, b : access m128i; c, d, mask : m128i)
      return m128i;
   pragma Inline_Always (mm_recursion);
   --  This function represents the recursion formula.
   function mm_recursion (a, b : access m128i; c, d, mask : m128i)
      return m128i
   is
      v, x, y, z : m128i;
   begin
--    x := mm_load_si128 (a);
      x := a.all;
--    y := mm_srli_epi32 (b.all, SR1);
      y := To_m128i (ia32_psrldi128 (To_v4si (b.all), SR1));
      z := mm_srli_si128 (c, SR2 * 8);
--    v := mm_slli_epi32 (d, SL1);
      v := To_m128i (ia32_pslldi128 (To_v4si (d), SL1));
      z := mm_xor_si128 (z, x);
      z := mm_xor_si128 (z, v);
      x := mm_slli_si128 (x, SL2 * 8);
      y := mm_and_si128 (y, mask);
      z := mm_xor_si128 (z, x);
      z := mm_xor_si128 (z, y);
      return z;
   end mm_recursion;

   --  implementation

   --  This function fills the internal state array with pseudorandom
   --  integers.
   procedure gen_rand_all (
      sfmt : in out w128_t_Array_N)
   is
      i : Integer;
      r, r1, r2, mask : m128i;
      sfmt_si : array (Natural) of aliased m128i;
      for sfmt_si'Address use sfmt'Address;
   begin
      mask := mm_set_epi32 (MSK4, MSK3, MSK2, MSK1);
--    r1 := mm_load_si128 (sfmt_si (N - 2)'Access);
      r1 := sfmt_si (N - 2);
--    r2 := mm_load_si128 (sfmt_si (N - 1)'Access);
      r2 := sfmt_si (N - 1);
      i := 0;
      while i < N - POS1 loop
         r := mm_recursion (
            sfmt_si (i)'Access,
            sfmt_si (i + POS1)'Access,
            r1,
            r2,
            mask);
--       mm_store_si128 (sfmt_si (i)'Access, r);
         sfmt_si (i) := r;
         r1 := r2;
         r2 := r;
         i := i + 1;
      end loop;
      while i < N loop
         r := mm_recursion (
            sfmt_si (i)'Access,
            sfmt_si (i + POS1 - N)'Access,
            r1,
            r2,
            mask);
--       mm_store_si128 (sfmt_si (i)'Access, r);
         sfmt_si (i) := r;
         r1 := r2;
         r2 := r;
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
      r, r1, r2, mask : m128i;
      sfmt_si : array (Natural) of aliased m128i;
      for sfmt_si'Address use sfmt'Address;
      array_si : array (Natural) of aliased m128i;
      for array_si'Address use Item'Address;
   begin
      mask := mm_set_epi32 (MSK4, MSK3, MSK2, MSK1);
--    r1 := mm_load_si128 (sfmt_si (N - 2)'Access);
      r1 := sfmt_si (N - 2);
--    r2 := mm_load_si128 (sfmt_si (N - 1)'Access);
      r2 := sfmt_si (N - 1);
      i := 0;
      while i < N - POS1 loop
         r := mm_recursion (
            sfmt_si (i)'Access,
            sfmt_si (i + POS1)'Access,
            r1,
            r2,
            mask);
--       mm_store_si128 (array_si (i)'Access, r);
         array_si (i) := r;
         r1 := r2;
         r2 := r;
         i := i + 1;
      end loop;
      while i < N loop
         r := mm_recursion (
            sfmt_si (i)'Access,
            array_si (i + POS1 - N)'Access,
            r1,
            r2,
            mask);
--       mm_store_si128 (array_si (i)'Access, r);
         array_si (i) := r;
         r1 := r2;
         r2 := r;
         i := i + 1;
      end loop;
      --  main loop
      while i < size - N loop
         r := mm_recursion (
            array_si (i - N)'Access,
            array_si (i + POS1 - N)'Access,
            r1,
            r2,
            mask);
--       mm_store_si128 (array_si (i)'Access, r);
         array_si (i) := r;
         r1 := r2;
         r2 := r;
         i := i + 1;
      end loop;
      j := 0;
      while j < 2 * N - size loop
--       r := mm_load_si128 (array_si (j + size - N)'Access);
         r := array_si (j + size - N);
--       mm_store_si128 (sfmt_si (j)'Access, r);
         sfmt_si (j) := r;
         j := j + 1;
      end loop;
      while i < size loop
         r := mm_recursion (
            array_si (i - N)'Access,
            array_si (i + POS1 - N)'Access,
            r1,
            r2,
            mask);
--       mm_store_si128 (array_si (i)'Access, r);
         array_si (i) := r;
--       mm_store_si128 (sfmt_si (j)'Access, r);
         sfmt_si (j) := r;
         j := j + 1;
         r1 := r2;
         r2 := r;
         i := i + 1;
      end loop;
   end gen_rand_array;

end Ada.Numerics.SFMT.Random.Inside;
