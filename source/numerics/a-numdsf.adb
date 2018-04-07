with Ada.Numerics.dSFMT.Generating;
with System.Formatting;
with System.Long_Long_Integer_Types;
with System.Random_Initiators;
with System.Storage_Elements;
package body Ada.Numerics.dSFMT is
   pragma Check_Policy (Validate => Ignore);
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type System.Storage_Elements.Storage_Offset;

   subtype Word_Unsigned is System.Long_Long_Integer_Types.Word_Unsigned;
   subtype Long_Long_Unsigned is
      System.Long_Long_Integer_Types.Long_Long_Unsigned;

   procedure memset (
      b : System.Address;
      c : Integer;
      n : System.Storage_Elements.Storage_Count)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memset";

   type Long_Boolean is new Boolean;
   for Long_Boolean'Size use Long_Integer'Size;

   function expect (exp, c : Long_Boolean) return Long_Boolean
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_expect";

   package Impl is new Generating;

   --  STATIC FUNCTIONS

   function ini_func1 (x : Unsigned_32) return Unsigned_32
      with Convention => Intrinsic;
   function ini_func2 (x : Unsigned_32) return Unsigned_32
      with Convention => Intrinsic;

   pragma Inline_Always (ini_func1);
   pragma Inline_Always (ini_func2);

   procedure gen_rand_array_c1o2 (
      dsfmt : in out State;
      Item : out w128_t_Array_1;
      size : Integer)
      with Convention => Intrinsic;
   procedure gen_rand_array_c0o1 (
      dsfmt : in out State;
      Item : out w128_t_Array_1;
      size : Integer)
      with Convention => Intrinsic;
   procedure gen_rand_array_o0c1 (
      dsfmt : in out State;
      Item : out w128_t_Array_1;
      size : Integer)
      with Convention => Intrinsic;
   procedure gen_rand_array_o0o1 (
      dsfmt : in out State;
      Item : out w128_t_Array_1;
      size : Integer)
      with Convention => Intrinsic;

   pragma Inline_Always (gen_rand_array_c1o2);
   pragma Inline_Always (gen_rand_array_c0o1);
   pragma Inline_Always (gen_rand_array_o0c1);
   pragma Inline_Always (gen_rand_array_o0o1);

   function idxof (i : Integer) return Integer
      with Convention => Intrinsic;
   pragma Inline_Always (idxof);

   procedure initial_mask (dsfmt : in out State);
   procedure period_certification (dsfmt : in out State);

   --  This function simulate a 32-bit array index overlapped to 64-bit array
   --    of LITTLE ENDIAN in BIG ENDIAN machine.
   function idxof (i : Integer) return Integer is
      type Unsigned is mod 2 ** Integer'Size;
   begin
      case System.Default_Bit_Order is
         when System.High_Order_First =>
            return Integer (Unsigned'Mod (i) xor 1);
         when System.Low_Order_First =>
            return i;
      end case;
   end idxof;

   --  This function fills the user-specified array with double precision
   --    floating point pseudorandom numbers of the IEEE 754 format.
   procedure gen_rand_array_c1o2 (
      dsfmt : in out State;
      Item : out w128_t_Array_1;
      size : Integer)
   is
      pragma Suppress (Index_Check);
      the_array : w128_t_Array (0 .. size - 1);
      for the_array'Address use Item'Address;
      i, j : Integer;
      lung : aliased w128_t := dsfmt.lung;
   begin
      Impl.do_recursion (
         the_array (0),
         dsfmt.status (0),
         dsfmt.status (POS1),
         lung);
      i := 1;
      while i < N - POS1 loop
         Impl.do_recursion (
            the_array (i),
            dsfmt.status (i),
            dsfmt.status (i + POS1),
            lung);
         i := i + 1;
      end loop;
      while i < N loop
         Impl.do_recursion (
            the_array (i),
            dsfmt.status (i),
            the_array (i - (N - POS1)),
            lung);
         i := i + 1;
      end loop;
      while i < size - N loop
         Impl.do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i - (N - POS1)),
            lung);
         i := i + 1;
      end loop;
      j := 0;
      while j < N - (size - N) loop
         dsfmt.status (j) := the_array (j + (size - N));
         j := j + 1;
      end loop;
      while i < size loop
         Impl.do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i - (N - POS1)),
            lung);
         dsfmt.status (j) := the_array (i);
         i := i + 1;
         j := j + 1;
      end loop;
      dsfmt.lung := lung;
   end gen_rand_array_c1o2;

   --  This function fills the user-specified array with double precision
   --    floating point pseudorandom numbers of the IEEE 754 format.
   procedure gen_rand_array_c0o1 (
      dsfmt : in out State;
      Item : out w128_t_Array_1;
      size : Integer)
   is
      pragma Suppress (Index_Check);
      the_array : w128_t_Array (0 .. size - 1);
      for the_array'Address use Item'Address;
      i, j : Integer;
      lung : aliased w128_t := dsfmt.lung;
   begin
      Impl.do_recursion (
         the_array (0),
         dsfmt.status (0),
         dsfmt.status (POS1),
         lung);
      i := 1;
      while i < N - POS1 loop
         Impl.do_recursion (
            the_array (i),
            dsfmt.status (i),
            dsfmt.status (i + POS1),
            lung);
         i := i + 1;
      end loop;
      while i < N loop
         Impl.do_recursion (
            the_array (i),
            dsfmt.status (i),
            the_array (i - (N - POS1)),
            lung);
         i := i + 1;
      end loop;
      while i < size - N loop
         Impl.do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i - (N - POS1)),
            lung);
         Impl.convert_c0o1 (the_array (i - N)); -- [0 .. size - 2 * N)
         i := i + 1;
      end loop;
      j := 0;
      while j < N - (size - N) loop
         dsfmt.status (j) := the_array (j + (size - N));
         j := j + 1;
      end loop;
      while i < size loop
         Impl.do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i - (N - POS1)),
            lung);
         dsfmt.status (j) := the_array (i);
         Impl.convert_c0o1 (the_array (i - N)); -- [size - 2 * N .. size - N)
         i := i + 1;
         j := j + 1;
      end loop;
      i := size - N;
      while i < size loop
         Impl.convert_c0o1 (the_array (i)); -- [size - N .. size)
         i := i + 1;
      end loop;
      dsfmt.lung := lung;
   end gen_rand_array_c0o1;

   --  This function fills the user-specified array with double precision
   --    floating point pseudorandom numbers of the IEEE 754 format.
   procedure gen_rand_array_o0c1 (
      dsfmt : in out State;
      Item : out w128_t_Array_1;
      size : Integer)
   is
      pragma Suppress (Index_Check);
      the_array : w128_t_Array (0 .. size - 1);
      for the_array'Address use Item'Address;
      i, j : Integer;
      lung : aliased w128_t := dsfmt.lung;
   begin
      Impl.do_recursion (
         the_array (0),
         dsfmt.status (0),
         dsfmt.status (POS1),
         lung);
      i := 1;
      while i < N - POS1 loop
         Impl.do_recursion (
            the_array (i),
            dsfmt.status (i),
            dsfmt.status (i + POS1),
            lung);
         i := i + 1;
      end loop;
      while i < N loop
         Impl.do_recursion (
            the_array (i),
            dsfmt.status (i),
            the_array (i - (N - POS1)),
            lung);
         i := i + 1;
      end loop;
      while i < size - N loop
         Impl.do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i - (N - POS1)),
            lung);
         Impl.convert_o0c1 (the_array (i - N));
         i := i + 1;
      end loop;
      j := 0;
      while j < N - (size - N) loop
         dsfmt.status (j) := the_array (j + (size - N));
         j := j + 1;
      end loop;
      while i < size loop
         Impl.do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i - (N - POS1)),
            lung);
         dsfmt.status (j) := the_array (i);
         Impl.convert_o0c1 (the_array (i - N));
         i := i + 1;
         j := j + 1;
      end loop;
      i := size - N;
      while i < size loop
         Impl.convert_o0c1 (the_array (i));
         i := i + 1;
      end loop;
      dsfmt.lung := lung;
   end gen_rand_array_o0c1;

   --  This function fills the user-specified array with double precision
   --    floating point pseudorandom numbers of the IEEE 754 format.
   procedure gen_rand_array_o0o1 (
      dsfmt : in out State;
      Item : out w128_t_Array_1;
      size : Integer)
   is
      pragma Suppress (Index_Check);
      the_array : w128_t_Array (0 .. size - 1);
      for the_array'Address use Item'Address;
      i, j : Integer;
      lung : aliased w128_t := dsfmt.lung;
   begin
      Impl.do_recursion (
         the_array (0),
         dsfmt.status (0),
         dsfmt.status (POS1),
         lung);
      i := 1;
      while i < N - POS1 loop
         Impl.do_recursion (
            the_array (i),
            dsfmt.status (i),
            dsfmt.status (i + POS1),
            lung);
         i := i + 1;
      end loop;
      while i < N loop
         Impl.do_recursion (
            the_array (i),
            dsfmt.status (i),
            the_array (i - (N - POS1)),
            lung);
         i := i + 1;
      end loop;
      while i < size - N loop
         Impl.do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i - (N - POS1)),
            lung);
         Impl.convert_o0o1 (the_array (i - N));
         i := i + 1;
      end loop;
      j := 0;
      while j < N - (size - N) loop
         dsfmt.status (j) := the_array (j + (size - N));
         j := j + 1;
      end loop;
      while i < size loop
         Impl.do_recursion (
            the_array (i),
            the_array (i - N),
            the_array (i - (N - POS1)),
            lung);
         dsfmt.status (j) := the_array (i);
         Impl.convert_o0o1 (the_array (i - N));
         i := i + 1;
         j := j + 1;
      end loop;
      i := size - N;
      while i < size loop
         Impl.convert_o0o1 (the_array (i));
         i := i + 1;
      end loop;
      dsfmt.lung := lung;
   end gen_rand_array_o0o1;

   --  This function represents a function used in the initialization by
   --    init_by_array
   function ini_func1 (x : Unsigned_32) return Unsigned_32 is
   begin
      return (x xor Interfaces.Shift_Right (x, 27)) * Unsigned_32'(1664525);
   end ini_func1;

   --  This function represents a function used in the initialization by
   --    init_by_array
   function ini_func2 (x : Unsigned_32) return Unsigned_32 is
   begin
      return (x xor Interfaces.Shift_Right (x, 27)) * Unsigned_32'(1566083941);
   end ini_func2;

   --  This function initializes the internal state array to fit the IEEE 754
   --    format.
   procedure initial_mask (dsfmt : in out State) is
      psfmt : Unsigned_64_Array (0 .. N * 2 - 1);
      for psfmt'Address use dsfmt.status'Address;
   begin
      for i in 0 .. N * 2 - 1 loop
         psfmt (i) := (psfmt (i) and LOW_MASK) or HIGH_CONST;
      end loop;
   end initial_mask;

   --  This function certificate the period of 2^{SFMT_MEXP}-1.
   procedure period_certification (dsfmt : in out State) is
      pcv : constant array (0 .. 1) of Unsigned_64 := (PCV1, PCV2);
      tmp : array (0 .. 1) of Unsigned_64;
      inner : Unsigned_64;
      work : Unsigned_64;
   begin
      tmp (0) := dsfmt.lung (0) xor FIX1;
      tmp (1) := dsfmt.lung (1) xor FIX2;
      inner := tmp (0) and pcv (0);
      inner := inner xor (tmp (1) and pcv (1));
      declare
         i : Natural := 32;
      begin
         while i > 0 loop
            inner := inner xor Interfaces.Shift_Right (inner, i);
            i := i / 2;
         end loop;
      end;
      inner := inner and 1;
      --  check OK
      if inner = 1 then
         return;
      end if;
      --  check NG, and modification
      if (PCV2 and 1) = 1 then
         dsfmt.lung (1) := dsfmt.lung (1) xor 1;
      else
         for i in reverse 0 .. 1 loop
            work := 1;
            for j in 0 .. 64 - 1 loop
               if (work and pcv (i)) /= 0 then
                  dsfmt.lung (i) := dsfmt.lung (i) xor work;
                  return;
               end if;
               work := Interfaces.Shift_Left (work, 1);
            end loop;
         end loop;
      end if;
   end period_certification;

   --  PUBLIC FUNCTIONS

   procedure dsfmt_gen_rand_all (dsfmt : in out State);

   procedure dsfmt_chk_init_gen_rand (
      dsfmt : in out State;
      seed : Unsigned_32);

   procedure dsfmt_chk_init_by_array (
      dsfmt : in out State;
      init_key : Unsigned_32_Array);

   --  This function fills the internal state array with double precision
   --    floating point pseudorandom numbers of the IEEE 754 format.
   procedure dsfmt_gen_rand_all (dsfmt : in out State) is
      pragma Suppress (Index_Check);
      i : Integer;
      lung : aliased w128_t := dsfmt.lung;
   begin
      Impl.do_recursion (
         dsfmt.status (0),
         dsfmt.status (0),
         dsfmt.status (POS1),
         lung);
      i := 1;
      while i < N - POS1 loop
         Impl.do_recursion (
            dsfmt.status (i),
            dsfmt.status (i),
            dsfmt.status (i + POS1),
            lung);
         i := i + 1;
      end loop;
      while i < N loop
         Impl.do_recursion (
            dsfmt.status (i),
            dsfmt.status (i),
            dsfmt.status (i - (N - POS1)),
            lung);
         i := i + 1;
      end loop;
      dsfmt.lung := lung;
   end dsfmt_gen_rand_all;

   --  This function initializes the internal state array with a 32-bit integer
   --    seed.
   procedure dsfmt_chk_init_gen_rand (
      dsfmt : in out State;
      seed : Unsigned_32) is
   begin
      --  make sure caller program is compiled with the same MEXP
      --  fprintf(stderr, "DSFMT_MEXP doesn't match with dSFMT.c\n");
      declare
         psfmt : Unsigned_32_Array (0 .. (N + 1) * 4 - 1); -- including lung
         for psfmt'Address use dsfmt.status'Address;
      begin
         psfmt (idxof (0)) := seed;
         for i in 1 .. (N + 1) * 4 - 1 loop
            psfmt (idxof (i)) :=
               1812433253
                  * (
                     psfmt (idxof (i - 1))
                     xor Interfaces.Shift_Right (psfmt (idxof (i - 1)), 30))
               + Unsigned_32'Mod (i);
         end loop;
      end;
      initial_mask (dsfmt);
      period_certification (dsfmt);
      dsfmt.idx := N64;
   end dsfmt_chk_init_gen_rand;

   --  This function initializes the internal state array, with an array of
   --    32-bit integers used as the seeds
   procedure dsfmt_chk_init_by_array (
      dsfmt : in out State;
      init_key : Unsigned_32_Array)
   is
      key_length : constant Natural := init_key'Length;
      count : Natural;
      r : Unsigned_32;
      lag : Natural;
      mid : Natural;
      size : constant Natural := (N + 1) * 4; -- pulmonary
   begin
      --  make sure caller program is compiled with the same MEXP
      --  fprintf(stderr, "DSFMT_MEXP doesn't match with dSFMT.c\n");
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

      memset (
         dsfmt.status'Address,
         16#8b#,
         System.Storage_Elements.Storage_Offset (
            (N + 1) * (128 / Standard'Storage_Unit))); -- including lung
      if key_length + 1 > size then
         count := key_length + 1;
      else
         count := size;
      end if;
      declare
         psfmt32 : Unsigned_32_Array (0 .. (N + 1) * 4 - 1); -- including lung
         for psfmt32'Address use dsfmt.status'Address;
      begin
         r :=
            ini_func1 (psfmt32 (idxof (0))
            xor psfmt32 (idxof (mid rem size))
            xor psfmt32 (idxof ((size - 1) rem size)));
         declare
            Index : constant Natural := idxof (mid rem size);
         begin
            psfmt32 (Index) := psfmt32 (Index) + r;
         end;
         r := r + Unsigned_32'Mod (key_length);
         declare
            Index : constant Natural := idxof ((mid + lag) rem size);
         begin
            psfmt32 (Index) := psfmt32 (Index) + r;
         end;
         psfmt32 (idxof (0)) := r;
         count := count - 1;
         declare
            i : Natural := 1;
         begin
            declare
               j : Natural := 0;
            begin
               while j < count and then j < key_length loop
                  r :=
                     ini_func1 (psfmt32 (idxof (i))
                     xor psfmt32 (idxof ((i + mid) rem size))
                     xor psfmt32 (idxof ((i + (size - 1)) rem size)));
                  declare
                     Index : constant Natural := idxof ((i + mid) rem size);
                  begin
                     psfmt32 (Index) := psfmt32 (Index) + r;
                  end;
                  r := r + init_key (j) + Unsigned_32'Mod (i);
                  declare
                     Index : constant Natural :=
                        idxof ((i + mid + lag) rem size);
                  begin
                     psfmt32 (Index) := psfmt32 (Index) + r;
                  end;
                  psfmt32 (idxof (i)) := r;
                  i := (i + 1) rem size;
                  j := j + 1;
               end loop;
               while j < count loop
                  r :=
                     ini_func1 (psfmt32 (idxof (i))
                     xor psfmt32 (idxof ((i + mid) rem size))
                     xor psfmt32 (idxof ((i + (size - 1)) rem size)));
                  declare
                     Index : constant Natural := idxof ((i + mid) rem size);
                  begin
                     psfmt32 (Index) := psfmt32 (Index) + r;
                  end;
                  r := r + Unsigned_32'Mod (i);
                  declare
                     Index : constant Natural :=
                        idxof ((i + mid + lag) rem size);
                  begin
                     psfmt32 (Index) := psfmt32 (Index) + r;
                  end;
                  psfmt32 (idxof (i)) := r;
                  i := (i + 1) rem size;
                  j := j + 1;
               end loop;
            end;
            for j in 0 .. size - 1 loop
               r :=
                  ini_func2 (psfmt32 (idxof (i))
                  + psfmt32 (idxof ((i + mid) rem size))
                  + psfmt32 (idxof ((i + (size - 1)) rem size)));
               declare
                  Index : constant Natural := idxof ((i + mid) rem size);
               begin
                  psfmt32 (Index) := psfmt32 (Index) xor r;
               end;
               r := r - Unsigned_32'Mod (i);
               declare
                  Index : constant Natural := idxof ((i + mid + lag) rem size);
               begin
                  psfmt32 (Index) := psfmt32 (Index) xor r;
               end;
               psfmt32 (idxof (i)) := r;
               i := (i + 1) rem size;
            end loop;
         end;
      end;
      initial_mask (dsfmt);
      period_certification (dsfmt);
      dsfmt.idx := N64;
   end dsfmt_chk_init_by_array;

   --  implementation

   --  This function returns the identification string.  The string shows the
   --    Mersenne exponent, and all parameters of this generator.
   --  equivalent to dsfmt_get_idstring
   function Id return String is
      --  e.g. "dSFMT2-19937:117-19:ffafffffffb3f-ffdfffc90fffd"
      Result : String (
         1 ..
         6 + 1 -- "dSFMT2-"
            + 6 + 1 + 4 + 1 + 2 + 1 -- "216091:1890-23:"
            + 13 + 1 + 13); -- "%.13x-%.13x"
      Last : Natural := 0;
      Error : Boolean;
   begin
      Result (Last + 1 .. Last + 7) := "dSFMT2-";
      Last := Last + 7;
      System.Formatting.Image (
         Word_Unsigned (MEXP),
         Result (Last + 1 .. Result'Last),
         Last,
         Error => Error);
      Result (Last + 1) := ':';
      Last := Last + 1;
      System.Formatting.Image (
         Word_Unsigned (POS1),
         Result (Last + 1 .. Result'Last),
         Last,
         Error => Error);
      Result (Last + 1) := '-';
      Last := Last + 1;
      System.Formatting.Image (
         Word_Unsigned (SL1),
         Result (Last + 1 .. Result'Last),
         Last,
         Error => Error);
      Result (Last + 1) := ':';
      Last := Last + 1;
      if Standard'Word_Size >= 64 then -- 52?
         System.Formatting.Image (
            Word_Unsigned (MSK1),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => 16,
            Set => System.Formatting.Lower_Case,
            Width => 13,
            Error => Error);
      else
         System.Formatting.Image (
            Long_Long_Unsigned (MSK1),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => 16,
            Set => System.Formatting.Lower_Case,
            Width => 13,
            Error => Error);
      end if;
      Result (Last + 1) := '-';
      Last := Last + 1;
      if Standard'Word_Size >= 64 then -- 52?
         System.Formatting.Image (
            Word_Unsigned (MSK2),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => 16,
            Set => System.Formatting.Lower_Case,
            Width => 13,
            Error => Error);
      else
         System.Formatting.Image (
            Long_Long_Unsigned (MSK2),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => 16,
            Set => System.Formatting.Lower_Case,
            Width => 13,
            Error => Error);
      end if;
      return Result (1 .. Last);
   end Id;

   --  This function generates and returns double precision pseudorandom number
   --    which distributes uniformly in the range [1, 2).  This is the
   --    primitive and faster than generating numbers in other ranges.
   --  equivalent to dsfmt_genrand_close1_open2
   function Random_1_To_Less_Than_2 (Gen : aliased in out Generator)
      return Long_Float
   is
      pragma Suppress (Index_Check);
      dsfmt : State renames Gen.dsfmt;
      idx : Integer := dsfmt.idx;
   begin
      if expect (Long_Boolean (idx >= N64), False) then
         dsfmt_gen_rand_all (dsfmt);
         idx := 0;
      end if;
      declare
         psfmt64 : Long_Float_Array (0 .. N * 2 - 1);
         for psfmt64'Address use dsfmt.status'Address;
         r : constant Long_Float := psfmt64 (idx);
      begin
         dsfmt.idx := idx + 1;
         return r;
      end;
   end Random_1_To_Less_Than_2;

   --  This function generates and returns double precision pseudorandom number
   --    which distributes uniformly in the range [0, 1).
   --  equivalent to dsfmt_genrand_close_open
   function Random_0_To_Less_Than_1 (Gen : aliased in out Generator)
      return Long_Float is
   begin
      return Random_1_To_Less_Than_2 (Gen) - 1.0;
   end Random_0_To_Less_Than_1;

   --  This function generates and returns double precision pseudorandom number
   --    which distributes uniformly in the range (0, 1].
   --  equivalent to dsfmt_genrand_open_close
   function Random_Greater_Than_0_To_1 (Gen : aliased in out Generator)
      return Long_Float is
   begin
      return 2.0 - Random_1_To_Less_Than_2 (Gen);
   end Random_Greater_Than_0_To_1;

   --  This function generates and returns double precision pseudorandom number
   --    which distributes uniformly in the range (0, 1).
   --  equivalent to dsfmt_genrand_open_open
   function Random_Greater_Than_0_To_Less_Than_1 (
      Gen : aliased in out Generator)
      return Long_Float
   is
      type union_r_Tag is (d, u);
      pragma Discard_Names (union_r_Tag);
      type union_r (Unchecked_Tag : union_r_Tag := d) is record
         case Unchecked_Tag is
            when d =>
               d : Long_Float;
            when u =>
               u : Unsigned_64;
         end case;
      end record;
      pragma Unchecked_Union (union_r);
      pragma Suppress_Initialization (union_r);
      r : union_r;
   begin
      r.d := Random_1_To_Less_Than_2 (Gen);
      r.u := r.u or 1;
      return r.d - 1.0;
   end Random_Greater_Than_0_To_Less_Than_1;

   --  This function generates double precision floating point pseudorandom
   --    numbers which distribute in the range [1, 2) to the specified array[]
   --    by one call.  The number of pseudorandom numbers is specified by the
   --    argument size, which must be at least (SFMT_MEXP / 128) * 2 and a
   --    multiple of two.  The function get_min_array_size() returns this
   --    minimum size.  The generation by this function is much faster than the
   --    following fill_array_xxx functions.
   --  equivalent to dsfmt_fill_array_close1_open2
   procedure Fill_Random_1_To_Less_Than_2 (
      Gen : aliased in out Generator;
      Item : out Long_Float_Array)
   is
      pragma Suppress (Range_Check);
      size : constant Natural := Item'Length;
   begin
      if Gen.dsfmt.idx /= N64 or else size rem 2 /= 0 or else size < N64 then
         --  This function can not be used after calling genrand_xxx functions,
         --    without initialization.
         for I in Item'Range loop
            Item (I) := Random_1_To_Less_Than_2 (Gen);
         end loop;
      end if;
      declare
         the_array : w128_t_Array (0 .. 0); -- size / 2 - 1
         for the_array'Address use Item'Address;
      begin
         gen_rand_array_c1o2 (Gen.dsfmt, the_array (0 .. 0), size / 2);
      end;
   end Fill_Random_1_To_Less_Than_2;

   --  This function generates double precision floating point pseudorandom
   --    numbers which distribute in the range [0, 1) to the specified array[]
   --    by one call.  This function is the same as fill_array_close1_open2()
   --    except the distribution range.
   --  equivalent to dsfmt_fill_array_close_open
   procedure Fill_Random_0_To_Less_Than_1 (
      Gen : aliased in out Generator;
      Item : out Long_Float_Array)
   is
      pragma Suppress (Range_Check);
      size : constant Natural := Item'Length;
   begin
      if Gen.dsfmt.idx /= N64 or else size rem 2 /= 0 or else size < N64 then
         for I in Item'Range loop
            Item (I) := Random_0_To_Less_Than_1 (Gen);
         end loop;
      end if;
      declare
         the_array : w128_t_Array (0 .. 0); -- size / 2 - 1
         for the_array'Address use Item'Address;
      begin
         gen_rand_array_c0o1 (Gen.dsfmt, the_array (0 .. 0), size / 2);
      end;
   end Fill_Random_0_To_Less_Than_1;

   --  This function generates double precision floating point pseudorandom
   --    numbers which distribute in the range (0, 1] to the specified array[]
   --    by one call.  This function is the same as fill_array_close1_open2()
   --    except the distribution range.
   --  equivalent to dsfmt_fill_array_open_close
   procedure Fill_Random_Greater_Than_0_To_1 (
      Gen : aliased in out Generator;
      Item : out Long_Float_Array)
   is
      pragma Suppress (Range_Check);
      size : constant Natural := Item'Length;
   begin
      if Gen.dsfmt.idx /= N64 or else size rem 2 /= 0 or else size < N64 then
         for I in Item'Range loop
            Item (I) := Random_Greater_Than_0_To_1 (Gen);
         end loop;
      end if;
      declare
         the_array : w128_t_Array (0 .. 0); -- size / 2 - 1
         for the_array'Address use Item'Address;
      begin
         gen_rand_array_o0c1 (Gen.dsfmt, the_array (0 .. 0), size / 2);
      end;
   end Fill_Random_Greater_Than_0_To_1;

   --  This function generates double precision floating point pseudorandom
   --    numbers which distribute in the range (0, 1) to the specified array[]
   --    by one call.  This function is the same as fill_array_close1_open2()
   --    except the distribution range.
   --  equivalent to dsfmt_fill_array_open_open
   procedure Fill_Random_Greater_Than_0_To_Less_Than_1 (
      Gen : aliased in out Generator;
      Item : out Long_Float_Array)
   is
      pragma Suppress (Range_Check);
      size : constant Natural := Item'Length;
   begin
      if Gen.dsfmt.idx /= N64 or else size rem 2 /= 0 or else size < N64 then
         for I in Item'Range loop
            Item (I) := Random_Greater_Than_0_To_Less_Than_1 (Gen);
         end loop;
      end if;
      declare
         the_array : w128_t_Array (0 .. 0); -- size / 2 - 1
         for the_array'Address use Item'Address;
      begin
         gen_rand_array_o0o1 (Gen.dsfmt, the_array (0 .. 0), size / 2);
      end;
   end Fill_Random_Greater_Than_0_To_Less_Than_1;

   function Initialize return Generator is
   begin
      return (dsfmt => Initialize);
   end Initialize;

   function Initialize (Initiator : Unsigned_32) return Generator is
   begin
      return (dsfmt => Initialize (Initiator));
   end Initialize;

   function Initialize (Initiator : Unsigned_32_Array) return Generator is
   begin
      return (dsfmt => Initialize (Initiator));
   end Initialize;

   procedure Reset (Gen : in out Generator) is
   begin
      Gen.dsfmt := Initialize;
   end Reset;

   procedure Reset (Gen : in out Generator; Initiator : Integer) is
   begin
      Gen.dsfmt := Initialize (Unsigned_32'Mod (Initiator));
   end Reset;

   function Initialize return State is
      Init : Unsigned_32_Array (0 .. N32 - 1);
   begin
      System.Random_Initiators.Get (
         Init'Address,
         Init'Size / Standard'Storage_Unit);
      return Initialize (Init);
   end Initialize;

   --  This function initializes the internal state array with a 32-bit integer
   --    seed.
   --  equivalent to dsfmt_init_gen_rand
   function Initialize (Initiator : Unsigned_32) return State is
   begin
      return Result : State do
         dsfmt_chk_init_gen_rand (Result, Initiator);
      end return;
   end Initialize;

   --  This function initializes the internal state array, with an array of
   --    32-bit integers used as the seeds.
   --  equivalent to dsfmt_init_by_array
   function Initialize (Initiator : Unsigned_32_Array) return State is
   begin
      return Result : State do
         dsfmt_chk_init_by_array (Result, Initiator);
      end return;
   end Initialize;

   procedure Save (Gen : Generator; To_State : out State) is
   begin
      To_State := Gen.dsfmt;
   end Save;

   procedure Reset (Gen : in out Generator; From_State : State) is
   begin
      Gen.dsfmt := From_State;
   end Reset;

   function Reset (From_State : State) return Generator is
   begin
      return (dsfmt => From_State);
   end Reset;

   function Image (Of_State : State) return String is
      procedure Put (To : out String; Item : w128_t);
      procedure Put (To : out String; Item : w128_t) is
         Last : Natural := To'First - 1;
      begin
         for I in w128_t'Range loop
            declare
               E : constant Unsigned_64 := Item (I);
               Previous_Last : constant Natural := Last;
               Error : Boolean;
            begin
               if Standard'Word_Size >= 64 then
                  System.Formatting.Image (
                     Word_Unsigned (E),
                     To (Previous_Last + 1 .. Previous_Last + 64 / 4),
                     Last,
                     Base => 16,
                     Width => 64 / 4,
                     Error => Error);
               else
                  System.Formatting.Image (
                     Long_Long_Unsigned (E),
                     To (Previous_Last + 1 .. Previous_Last + 64 / 4),
                     Last,
                     Base => 16,
                     Width => 64 / 4,
                     Error => Error);
               end if;
               pragma Check (Validate,
                  Check => not Error and then Last = Previous_Last + 64 / 4);
            end;
            Last := Last + 1;
            To (Last) := ':';
         end loop;
      end Put;
      procedure Put (To : out String; Item : Integer);
      procedure Put (To : out String; Item : Integer) is
         Error : Boolean;
         Last : Natural;
      begin
         System.Formatting.Image (
            Word_Unsigned (Item),
            To,
            Last,
            Base => 16,
            Width => 32 / 4,
            Error => Error);
         pragma Check (Validate, not Error and then Last = To'Last);
      end Put;
      Last : Natural := 0;
   begin
      return Result : String (1 .. Max_Image_Width) do
         for I in w128_t_Array_N'Range loop
            declare
               Previous_Last : constant Natural := Last;
            begin
               Last := Last + 2 * (64 / 4 + 1);
               Put (Result (Previous_Last + 1 .. Last), Of_State.status (I));
            end;
         end loop;
         declare
            Previous_Last : constant Natural := Last;
         begin
            Last := Last + 2 * (64 / 4 + 1);
            Put (Result (Previous_Last + 1 .. Last), Of_State.lung);
         end;
         Put (Result (Last + 1 .. Result'Last), Of_State.idx);
      end return;
   end Image;

   function Value (Coded_State : String) return State is
      procedure Get (From : String; Item : out w128_t);
      procedure Get (From : String; Item : out w128_t) is
         Last : Natural := From'First - 1;
      begin
         for I in w128_t'Range loop
            declare
               Previous_Last : constant Natural := Last;
               E : Unsigned_64;
               Error : Boolean;
            begin
               if Standard'Word_Size >= 64 then
                  System.Formatting.Value (
                     From (Previous_Last + 1 .. Previous_Last + 64 / 4),
                     Last,
                     Word_Unsigned (E),
                     Base => 16,
                     Error => Error);
               else
                  System.Formatting.Value (
                     From (Previous_Last + 1 .. Previous_Last + 64 / 4),
                     Last,
                     Long_Long_Unsigned (E),
                     Base => 16,
                     Error => Error);
               end if;
               if Error or else Last /= Previous_Last + 64 / 4 then
                  raise Constraint_Error;
               end if;
               Item (I) := E;
            end;
            Last := Last + 1;
            if From (Last) /= ':' then
               raise Constraint_Error;
            end if;
         end loop;
      end Get;
      procedure Get (From : String; Item : out Integer);
      procedure Get (From : String; Item : out Integer) is
         Last : Positive;
         Error : Boolean;
      begin
         System.Formatting.Value (
            From,
            Last,
            Word_Unsigned (Item),
            Base => 16,
            Error => Error);
         if Error or else Last /= From'Last or else Item not in 0 .. N64 then
            raise Constraint_Error;
         end if;
      end Get;
   begin
      if Coded_State'Length /= Max_Image_Width then
         raise Constraint_Error;
      end if;
      return Result : State do
         declare
            Last : Natural := Coded_State'First - 1;
         begin
            for I in w128_t_Array_N'Range loop
               declare
                  Previous_Last : constant Natural := Last;
               begin
                  Last := Last + 2 * (64 / 4 + 1);
                  Get (
                     Coded_State (Previous_Last + 1 .. Last),
                     Result.status (I));
               end;
            end loop;
            declare
               Previous_Last : constant Natural := Last;
            begin
               Last := Last + 2 * (64 / 4 + 1);
               Get (Coded_State (Previous_Last + 1 .. Last), Result.lung);
            end;
            Get (Coded_State (Last + 1 .. Coded_State'Last), Result.idx);
         end;
      end return;
   end Value;

end Ada.Numerics.dSFMT;
