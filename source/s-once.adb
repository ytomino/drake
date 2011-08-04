package body System.Once is
   pragma Suppress (All_Checks);

   Yet : constant := 0;
   Start : constant := 1;
   Done : constant := 2;

   function sync_val_compare_and_swap (
      A1 : not null access Flag;
      A2 : Flag;
      A3 : Flag)
      return Flag;
   pragma Import (Intrinsic, sync_val_compare_and_swap,
      "__sync_val_compare_and_swap_1");

   procedure Initialize (
      Flag : not null access Once.Flag;
      Process : not null access procedure) is
   begin
      case sync_val_compare_and_swap (Flag, Yet, Start) is
         when Yet => -- succeeded to swap
            pragma Assert (Flag.all = Start);
            Process.all;
            Flag.all := Done;
         when Start => -- wait
            loop
               Yield_Hook.all;
               exit when Flag.all = Done;
            end loop;
         when others => -- done
            null;
      end case;
   end Initialize;

end System.Once;
