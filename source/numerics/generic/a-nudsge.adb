package body Ada.Numerics.dSFMT.Generating is
   --  no SIMD version
   use type Interfaces.Unsigned_64;

   procedure do_recursion (
      r : aliased out w128_t;
      a, b : aliased w128_t;
      lung : aliased in out w128_t)
   is
      t0, t1, L0, L1 : Unsigned_64;
   begin
      t0 := a (0);
      t1 := a (1);
      L0 := lung (0);
      L1 := lung (1);
      lung (0) :=
         Interfaces.Shift_Left (t0, SL1)
         xor Interfaces.Shift_Right (L1, 32)
         xor Interfaces.Shift_Left (L1, 32)
         xor b (0);
      lung (1) :=
         Interfaces.Shift_Left (t1, SL1)
         xor Interfaces.Shift_Right (L0, 32)
         xor Interfaces.Shift_Left (L0, 32)
         xor b (1);
      r (0) :=
         Interfaces.Shift_Right (lung (0), SR) xor (lung (0) and MSK1) xor t0;
      r (1) :=
         Interfaces.Shift_Right (lung (1), SR) xor (lung (1) and MSK2) xor t1;
   end do_recursion;

   procedure convert_c0o1 (w : aliased in out w128_t) is
      d : Long_Float_Array (0 .. 1);
      for d'Address use w'Address;
   begin
      d (0) := d (0) - 1.0;
      d (1) := d (1) - 1.0;
   end convert_c0o1;

   procedure convert_o0c1 (w : aliased in out w128_t) is
      d : Long_Float_Array (0 .. 1);
      for d'Address use w'Address;
   begin
      d (0) := 2.0 - d (0);
      d (1) := 2.0 - d (1);
   end convert_o0c1;

   procedure convert_o0o1 (w : aliased in out w128_t) is
      d : Long_Float_Array (0 .. 1);
      for d'Address use w'Address;
   begin
      w (0) := w (0) or 1;
      w (1) := w (1) or 1;
      d (0) := d (0) - 1.0;
      d (1) := d (1) - 1.0;
   end convert_o0o1;

end Ada.Numerics.dSFMT.Generating;
