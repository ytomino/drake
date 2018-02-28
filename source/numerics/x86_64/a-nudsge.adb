with Ada.Unchecked_Conversion;
package body Ada.Numerics.dSFMT.Generating is
   --  SSE2 version
   use type Interfaces.Unsigned_64;

   type v2df is array (0 .. 1) of Long_Float;
   for v2df'Alignment use 16;
   pragma Machine_Attribute (v2df, "vector_type");
   pragma Machine_Attribute (v2df, "may_alias");
   pragma Suppress_Initialization (v2df);

   function mm_add_pd (a, b : v2df) return v2df
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_addpd";
   function mm_sub_pd (a, b : v2df) return v2df
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_subpd";

   type v4si is array (0 .. 3) of Unsigned_32;
   for v4si'Alignment use 16;
   pragma Machine_Attribute (v4si, "vector_type");
   pragma Machine_Attribute (v4si, "may_alias");
   pragma Suppress_Initialization (v4si);

   function mm_shuffle_epi32 (a : v4si; b : Integer) return v4si
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_pshufd";

   type v2di is array (0 .. 1) of Unsigned_64;
   for v2di'Alignment use 16;
   pragma Machine_Attribute (v2di, "vector_type");
   pragma Machine_Attribute (v2di, "may_alias");
   pragma Suppress_Initialization (v2di);

   function mm_and_si128 (a, b : v2di) return v2di
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_pand128";
   function mm_or_si128 (a, b : v2di) return v2di
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_por128";
   function mm_xor_si128 (a, b : v2di) return v2di
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_pxor128";
   function mm_slli_epi64 (a : v2di; b : Integer) return v2di
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_psllqi128";
   function mm_srli_epi64 (a : v2di; b : Integer) return v2di
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_ia32_psrlqi128";

   function To_v2df is new Unchecked_Conversion (v2di, v2df);
   function To_v2df is new Unchecked_Conversion (w128_t, v2df);
   function To_v2di is new Unchecked_Conversion (v4si, v2di);
   function To_v2di is new Unchecked_Conversion (w128_t, v2di);
   function To_w128_t is new Unchecked_Conversion (v2df, w128_t);
   function To_w128_t is new Unchecked_Conversion (v2di, w128_t);

   SSE2_SHUFF : constant := 16#1b#;

   --  1 in 64bit for sse2
   sse2_int_one : constant v2di := (1, 1);
   --  2.0 double for sse2
   sse2_double_two : constant v2df := (2.0, 2.0);
   --  -1.0 double for sse2
   sse2_double_m_one : constant v2df := (-1.0, -1.0);

   --  implementation

   procedure do_recursion (
      r : aliased out w128_t;
      a, b : aliased w128_t;
      lung : aliased in out w128_t)
   is
      type v4si_Access is access all v4si;
      type w128_t_Access is access all w128_t;
      function To_v4si is
         new Unchecked_Conversion (w128_t_Access, v4si_Access);
      --  mask data for sse2
      sse2_param_mask : constant v2di := (MSK1, MSK2);
      v, w, x, y, z : v2di;
   begin
      x := To_v2di (a);
      z := mm_slli_epi64 (x, SL1);
      y := To_v2di (mm_shuffle_epi32 (To_v4si (lung'Access).all, SSE2_SHUFF));
      z := mm_xor_si128 (z, To_v2di (b));
      y := mm_xor_si128 (y, z);

      v := mm_srli_epi64 (y, SR);
      w := mm_and_si128 (y, sse2_param_mask);
      v := mm_xor_si128 (v, x);
      v := mm_xor_si128 (v, w);
      r := To_w128_t (v);
      lung := To_w128_t (y);
   end do_recursion;

   procedure convert_c0o1 (w : aliased in out w128_t) is
   begin
      w := To_w128_t (mm_add_pd (To_v2df (w), sse2_double_m_one));
   end convert_c0o1;

   procedure convert_o0c1 (w : aliased in out w128_t) is
   begin
      w := To_w128_t (mm_sub_pd (sse2_double_two, To_v2df (w)));
   end convert_o0c1;

   procedure convert_o0o1 (w : aliased in out w128_t) is
   begin
      w := To_w128_t (
         mm_add_pd (
            To_v2df (mm_or_si128 (To_v2di (w), sse2_int_one)),
            sse2_double_m_one));
   end convert_o0o1;

end Ada.Numerics.dSFMT.Generating;
