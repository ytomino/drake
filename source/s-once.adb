with System.Storage_Barriers;
with System.Synchronous_Control;
package body System.Once is
   pragma Suppress (All_Checks);

   Yet : constant := 0;
   Start : constant := 1;
   Done : constant := 2;

   function atomic_load (
      ptr : not null access constant Flag;
      memorder : Integer := Storage_Barriers.ATOMIC_ACQUIRE)
      return Flag
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_1";

   procedure atomic_store (
      ptr : not null access Flag;
      val : Flag;
      memorder : Integer := Storage_Barriers.ATOMIC_RELEASE)
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_store_1";

   function atomic_compare_exchange (
      ptr : not null access Flag;
      expected : not null access Flag;
      desired : Flag;
      weak : Boolean := False;
      success_memorder : Integer := Storage_Barriers.ATOMIC_ACQ_REL;
      failure_memorder : Integer := Storage_Barriers.ATOMIC_ACQUIRE)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__atomic_compare_exchange_1";

   --  implementation

   procedure Initialize (
      Flag : not null access Once.Flag;
      Process : not null access procedure)
   is
      Expected : aliased Once.Flag := Yet;
   begin
      if atomic_compare_exchange (Flag, Expected'Access, Start) then
         --  succeeded to swap
         Process.all;
         atomic_store (Flag, Done);
      elsif Expected = Start then
         --  wait until Flag = Done
         loop
            Synchronous_Control.Yield;
            exit when atomic_load (Flag) = Done;
         end loop;
      end if;
      --  Flag = Done
   end Initialize;

end System.Once;
