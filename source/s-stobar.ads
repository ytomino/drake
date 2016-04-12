pragma License (Unrestricted);
--  implementation unit
with System.Storage_Elements;
package System.Storage_Barriers is
   pragma Pure;

   --  synchronization modes

   ATOMIC_RELAXED : constant := 0;
   ATOMIC_CONSUME : constant := 1;
   ATOMIC_ACQUIRE : constant := 2;
   ATOMIC_RELEASE : constant := 3;
   ATOMIC_ACQ_REL : constant := 4;
   ATOMIC_SEQ_CST : constant := 5;
--  ATOMIC_HLE_ACQUIRE : constant := 65536;
--  ATOMIC_HLE_RELEASE : constant := 131072;

   --  atomic 1-bit value

   type Flag is mod 2;
   for Flag'Size use 8;
   pragma Atomic (Flag);

   function atomic_load (
      ptr : not null access constant Flag;
      memorder : Integer := ATOMIC_ACQUIRE)
      return Flag
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_1";

   procedure atomic_store (
      ptr : not null access Flag;
      val : Flag;
      memorder : Integer := ATOMIC_RELEASE)
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_store_1";

   function atomic_exchange (
      ptr : not null access Flag;
      val : Flag;
      memorder : Integer := ATOMIC_ACQ_REL)
      return Flag
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_exchange_1";

   function atomic_compare_exchange (
      ptr : not null access Flag;
      expected : not null access Flag;
      desired : Flag;
      weak : Boolean := False;
      success_memorder : Integer := ATOMIC_ACQ_REL;
      failure_memorder : Integer := ATOMIC_ACQUIRE)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__atomic_compare_exchange_1";

   function atomic_test_and_set (
      ptr : not null access Flag;
      memorder : Integer := ATOMIC_ACQ_REL)
      return Boolean
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_test_and_set";

   procedure atomic_clear (
      ptr : not null access Flag;
      memorder : Integer := ATOMIC_RELEASE)
      with Import, Convention => Intrinsic, External_Name => "__atomic_clear";

   --  fences

   procedure atomic_thread_fence (
      memorder : Integer := ATOMIC_SEQ_CST)
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_thread_fence";

   procedure atomic_signal_fence (
      memorder : Integer := ATOMIC_SEQ_CST)
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_signal_fence";

   --  queries

   function atomic_always_lock_free (
      size : Storage_Elements.Storage_Count;
      ptr : Address)
      return Boolean
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_always_lock_free";

   function atomic_is_lock_free (
      size : Storage_Elements.Storage_Count;
      ptr : Address)
      return Boolean
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_is_lock_free";

end System.Storage_Barriers;
