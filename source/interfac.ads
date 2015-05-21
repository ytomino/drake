pragma License (Unrestricted);
package Interfaces is
   pragma Pure;

   type Integer_8 is range -2 ** 7 .. 2 ** 7 - 1; -- 2's complement
   for Integer_8'Size use 8;
   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;
   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Integer_32'Size use 32;
   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for Integer_64'Size use 64;

   type Unsigned_8 is mod 2 ** 8;
   for Unsigned_8'Size use 8;
   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;
   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;
   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;

   function Shift_Left (Value : Unsigned_8; Amount : Natural)
      return Unsigned_8;
   function Shift_Left (Value : Unsigned_16; Amount : Natural)
      return Unsigned_16;
   function Shift_Left (Value : Unsigned_32; Amount : Natural)
      return Unsigned_32;
   function Shift_Left (Value : Unsigned_64; Amount : Natural)
      return Unsigned_64;
   pragma Import (Intrinsic, Shift_Left);

   function Shift_Right (Value : Unsigned_8; Amount : Natural)
      return Unsigned_8;
   function Shift_Right (Value : Unsigned_16; Amount : Natural)
      return Unsigned_16;
   function Shift_Right (Value : Unsigned_32; Amount : Natural)
      return Unsigned_32;
   function Shift_Right (Value : Unsigned_64; Amount : Natural)
      return Unsigned_64;
   pragma Import (Intrinsic, Shift_Right);

   function Shift_Right_Arithmetic (Value : Unsigned_8; Amount : Natural)
      return Unsigned_8;
   function Shift_Right_Arithmetic (Value : Unsigned_16; Amount : Natural)
      return Unsigned_16;
   function Shift_Right_Arithmetic (Value : Unsigned_32; Amount : Natural)
      return Unsigned_32;
   function Shift_Right_Arithmetic (Value : Unsigned_64; Amount : Natural)
      return Unsigned_64;
   pragma Import (Intrinsic, Shift_Right_Arithmetic);

   function Rotate_Left (Value : Unsigned_8; Amount : Natural)
      return Unsigned_8;
   function Rotate_Left (Value : Unsigned_16; Amount : Natural)
      return Unsigned_16;
   function Rotate_Left (Value : Unsigned_32; Amount : Natural)
      return Unsigned_32;
   function Rotate_Left (Value : Unsigned_64; Amount : Natural)
      return Unsigned_64;
   pragma Import (Intrinsic, Rotate_Left);

   function Rotate_Right (Value : Unsigned_8; Amount : Natural)
      return Unsigned_8;
   function Rotate_Right (Value : Unsigned_16; Amount : Natural)
      return Unsigned_16;
   function Rotate_Right (Value : Unsigned_32; Amount : Natural)
      return Unsigned_32;
   function Rotate_Right (Value : Unsigned_64; Amount : Natural)
      return Unsigned_64;
   pragma Import (Intrinsic, Rotate_Right);

   --  extended from here
   --  Builtin-functions of gcc.

   function atomic_load (A1 : not null access Integer_8; Model : Natural)
      return Integer_8
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_1";
   function atomic_load (A1 : not null access Integer_16; Model : Natural)
      return Integer_16
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_2";
   function atomic_load (A1 : not null access Integer_32; Model : Natural)
      return Integer_32
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_4";
   function atomic_load (A1 : not null access Integer_64; Model : Natural)
      return Integer_64
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_8";

   function atomic_load (A1 : not null access Unsigned_8; Model : Natural)
      return Unsigned_8
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_1";
   function atomic_load (A1 : not null access Unsigned_16; Model : Natural)
      return Unsigned_16
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_2";
   function atomic_load (A1 : not null access Unsigned_32; Model : Natural)
      return Unsigned_32
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_4";
   function atomic_load (A1 : not null access Unsigned_64; Model : Natural)
      return Unsigned_64
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_8";

   procedure sync_add_and_fetch (
      A1 : not null access Integer_8;
      A2 : Integer_8)
      with Import,
         Convention => Intrinsic, External_Name => "__sync_add_and_fetch_1";
   procedure sync_add_and_fetch (
      A1 : not null access Integer_16;
      A2 : Integer_16)
      with Import,
         Convention => Intrinsic, External_Name => "__sync_add_and_fetch_2";
   procedure sync_add_and_fetch (
      A1 : not null access Integer_32;
      A2 : Integer_32)
      with Import,
         Convention => Intrinsic, External_Name => "__sync_add_and_fetch_4";
   procedure sync_add_and_fetch (
      A1 : not null access Integer_64;
      A2 : Integer_64)
      with Import,
         Convention => Intrinsic, External_Name => "__sync_add_and_fetch_8";

   procedure sync_add_and_fetch (
      A1 : not null access Unsigned_8;
      A2 : Unsigned_8)
      with Import,
         Convention => Intrinsic, External_Name => "__sync_add_and_fetch_1";
   procedure sync_add_and_fetch (
      A1 : not null access Unsigned_16;
      A2 : Unsigned_16)
      with Import,
         Convention => Intrinsic, External_Name => "__sync_add_and_fetch_2";
   procedure sync_add_and_fetch (
      A1 : not null access Unsigned_32;
      A2 : Unsigned_32)
      with Import,
         Convention => Intrinsic, External_Name => "__sync_add_and_fetch_4";
   procedure sync_add_and_fetch (
      A1 : not null access Unsigned_64;
      A2 : Unsigned_64)
      with Import,
         Convention => Intrinsic, External_Name => "__sync_add_and_fetch_8";

   function sync_sub_and_fetch (
      A1 : not null access Integer_8;
      A2 : Integer_8)
      return Integer_8
      with Import,
         Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_1";
   function sync_sub_and_fetch (
      A1 : not null access Integer_16;
      A2 : Integer_16)
      return Integer_16
      with Import,
         Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_2";
   function sync_sub_and_fetch (
      A1 : not null access Integer_32;
      A2 : Integer_32)
      return Integer_32
      with Import,
         Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_4";
   function sync_sub_and_fetch (
      A1 : not null access Integer_64;
      A2 : Integer_64)
      return Integer_64
      with Import,
         Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_8";

   function sync_sub_and_fetch (
      A1 : not null access Unsigned_8;
      A2 : Unsigned_8)
      return Unsigned_8
      with Import,
         Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_1";
   function sync_sub_and_fetch (
      A1 : not null access Unsigned_16;
      A2 : Unsigned_16)
      return Unsigned_16
      with Import,
         Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_2";
   function sync_sub_and_fetch (
      A1 : not null access Unsigned_32;
      A2 : Unsigned_32)
      return Unsigned_32
      with Import,
         Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_4";
   function sync_sub_and_fetch (
      A1 : not null access Unsigned_64;
      A2 : Unsigned_64)
      return Unsigned_64
      with Import,
         Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_8";

   function sync_bool_compare_and_swap (
      A1 : not null access Integer_8;
      A2 : Integer_8;
      A3 : Integer_8)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_bool_compare_and_swap_1";
   function sync_bool_compare_and_swap (
      A1 : not null access Integer_16;
      A2 : Integer_16;
      A3 : Integer_16)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_bool_compare_and_swap_2";
   function sync_bool_compare_and_swap (
      A1 : not null access Integer_32;
      A2 : Integer_32;
      A3 : Integer_32)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_bool_compare_and_swap_4";
   function sync_bool_compare_and_swap (
      A1 : not null access Integer_64;
      A2 : Integer_64;
      A3 : Integer_64)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_bool_compare_and_swap_8";

   function sync_bool_compare_and_swap (
      A1 : not null access Unsigned_8;
      A2 : Unsigned_8;
      A3 : Unsigned_8)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_bool_compare_and_swap_1";
   function sync_bool_compare_and_swap (
      A1 : not null access Unsigned_16;
      A2 : Unsigned_16;
      A3 : Unsigned_16)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_bool_compare_and_swap_2";
   function sync_bool_compare_and_swap (
      A1 : not null access Unsigned_32;
      A2 : Unsigned_32;
      A3 : Unsigned_32)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_bool_compare_and_swap_4";
   function sync_bool_compare_and_swap (
      A1 : not null access Unsigned_64;
      A2 : Unsigned_64;
      A3 : Unsigned_64)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_bool_compare_and_swap_8";

   function sync_val_compare_and_swap (
      A1 : not null access Integer_8;
      A2 : Integer_8;
      A3 : Integer_8)
      return Integer_8
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_val_compare_and_swap_1";
   function sync_val_compare_and_swap (
      A1 : not null access Integer_16;
      A2 : Integer_16;
      A3 : Integer_16)
      return Integer_16
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_val_compare_and_swap_2";
   function sync_val_compare_and_swap (
      A1 : not null access Integer_32;
      A2 : Integer_32;
      A3 : Integer_32)
      return Integer_32
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_val_compare_and_swap_4";
   function sync_val_compare_and_swap (
      A1 : not null access Integer_64;
      A2 : Integer_64;
      A3 : Integer_64)
      return Integer_64
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_val_compare_and_swap_8";

   function sync_val_compare_and_swap (
      A1 : not null access Unsigned_8;
      A2 : Unsigned_8;
      A3 : Unsigned_8)
      return Unsigned_8
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_val_compare_and_swap_1";
   function sync_val_compare_and_swap (
      A1 : not null access Unsigned_16;
      A2 : Unsigned_16;
      A3 : Unsigned_16)
      return Unsigned_16
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_val_compare_and_swap_2";
   function sync_val_compare_and_swap (
      A1 : not null access Unsigned_32;
      A2 : Unsigned_32;
      A3 : Unsigned_32)
      return Unsigned_32
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_val_compare_and_swap_4";
   function sync_val_compare_and_swap (
      A1 : not null access Unsigned_64;
      A2 : Unsigned_64;
      A3 : Unsigned_64)
      return Unsigned_64
      with Import,
         Convention => Intrinsic,
         External_Name => "__sync_val_compare_and_swap_8";

end Interfaces;
