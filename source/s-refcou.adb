package body System.Reference_Counting is
   pragma Suppress (All_Checks);

   procedure sync_add_and_fetch_32 (
      A1 : not null access Counter;
      A2 : Counter);
   pragma Import (Intrinsic, sync_add_and_fetch_32, "__sync_add_and_fetch_4");

   function sync_sub_and_fetch_32 (
      A1 : not null access Counter;
      A2 : Counter)
      return Counter;
   pragma Import (Intrinsic, sync_sub_and_fetch_32, "__sync_sub_and_fetch_4");

   function sync_bool_compare_and_swap (
      A1 : not null access Natural;
      A2 : Natural;
      A3 : Natural)
      return Boolean;
   function sync_bool_compare_and_swap (
      A1 : not null access Natural;
      A2 : Natural;
      A3 : Natural)
      return Boolean
   is
      pragma Compile_Time_Error (
         Integer'Size /= 32 and then Integer'Size /= 64,
         "Integer'Size is neither 32 nor 64");
   begin
      if Integer'Size = 32 then
         declare
            function sync_bool_compare_and_swap_32 (
               A1 : not null access Natural;
               A2 : Integer;
               A3 : Integer)
               return Boolean;
            pragma Import (Intrinsic, sync_bool_compare_and_swap_32,
               "__sync_bool_compare_and_swap_4");
         begin
            return sync_bool_compare_and_swap_32 (A1, A2, A3);
         end;
      else
         declare
            function sync_bool_compare_and_swap_64 (
               A1 : not null access Natural;
               A2 : Integer;
               A3 : Integer)
               return Boolean;
            pragma Import (Intrinsic, sync_bool_compare_and_swap_64,
               "__sync_bool_compare_and_swap_8");
         begin
            return sync_bool_compare_and_swap_64 (A1, A2, A3);
         end;
      end if;
   end sync_bool_compare_and_swap;

   --  implementation

   function Shared (Count : Counter) return Boolean is
   begin
      return Count > 1; -- static is True
   end Shared;

   --  not null because using sentinel (that means empty data block)

   procedure Adjust (Reference_Count : not null access Counter) is
   begin
      if Reference_Count.all /= Static then
         sync_add_and_fetch_32 (Reference_Count, 1);
      end if;
   end Adjust;

   procedure Assign (
      Target : not null access Address;
      Target_Reference_Count : not null access Counter;
      Source : not null access constant Address;
      Source_Reference_Count : not null access Counter;
      Free : not null access procedure (Object : Address)) is
   begin
      if Target.all /= Source.all then
         Clear (Target, Target_Reference_Count, Free);
         Target.all := Source.all;
         Adjust (Source_Reference_Count);
      end if;
   end Assign;

   procedure Clear (
      Target : not null access Address;
      Reference_Count : not null access Counter;
      Free : not null access procedure (Object : Address)) is
   begin
      if Reference_Count.all /= Static then
         if sync_sub_and_fetch_32 (Reference_Count, 1) = 0 then
            Free (Target.all);
         end if;
      end if;
   end Clear;

   procedure Move (
      Target : not null access Address;
      Target_Reference_Count : not null access Counter;
      Source : not null access Address;
      Sentinel : Address;
      Free : not null access procedure (Object : Address)) is
   begin
      if Target.all /= Source.all then
         Clear (Target, Target_Reference_Count, Free);
         Target.all := Source.all;
         Source.all := Sentinel;
      end if;
   end Move;

   procedure Unique (
      Target : not null access Address;
      Target_Reference_Count : not null access Counter;
      Target_Length : Natural;
      Target_Capacity : Natural;
      Max_Length : Natural;
      Capacity : Natural;
      Sentinel : Address;
      Copy : not null access procedure (
         Target : out Address;
         Source : Address;
         Length : Natural;
         Max_Length : Natural;
         Capacity : Natural);
      Free : not null access procedure (Object : Address)) is
   begin
      if Capacity /= Target_Capacity
         or else ( -- static (excluding Sentinel) is True
            Target.all /= Sentinel
            and then Target_Reference_Count.all > 1)
      then
         declare
            Old : aliased Address := Target.all;
            New_Capacity : constant Natural :=
               Integer'Max (Capacity, Target_Length);
         begin
            if New_Capacity = 0 then
               Target.all := Sentinel;
            else
               Copy (
                  Target.all,
                  Old,
                  Target_Length,
                  Max_Length,
                  New_Capacity);
            end if;
            Clear (Old'Access, Target_Reference_Count, Free => Free);
         end;
      end if;
   end Unique;

   procedure Set_Length (
      Target : not null access Address;
      Target_Reference_Count : not null access Counter;
      Target_Length : Natural;
      Target_Max_Length : not null access Natural;
      Target_Capacity : Natural;
      New_Length : Natural;
      Sentinel : Address;
      Copy : not null access procedure (
         Target : out Address;
         Source : Address;
         Length : Natural;
         Max_Length : Natural;
         Capacity : Natural);
      Free : not null access procedure (Object : Address)) is
   begin
      if New_Length > Target_Length then
         --  inscreasing
         if New_Length > Target_Capacity then
            --  expanding
            declare
               New_Capacity : constant Natural :=
                  Integer'Max (Target_Capacity * 2, New_Length);
            begin
               Unique (
                  Target => Target,
                  Target_Reference_Count => Target_Reference_Count,
                  Target_Length => Target_Length,
                  Target_Capacity => Target_Capacity,
                  Max_Length => New_Length,
                  Capacity => New_Capacity,
                  Sentinel => Sentinel,
                  Copy => Copy, -- Copy should set Max_Length
                  Free => Free);
            end;
         else
            --  try to use reserved area
            if sync_bool_compare_and_swap (
               Target_Max_Length,
               Target_Length,
               New_Length)
            then
               null;
            elsif Target_Reference_Count.all > 1 then
               Unique (
                  Target => Target,
                  Target_Reference_Count => Target_Reference_Count,
                  Target_Length => Target_Length,
                  Target_Capacity => Target_Capacity,
                  Max_Length => New_Length,
                  Capacity => Target_Capacity, -- keep Capacity
                  Sentinel => Sentinel,
                  Copy => Copy, -- Copy should set Max_Length
                  Free => Free);
            else -- reference count = 1
               Target_Max_Length.all := New_Length;
            end if;
         end if;
      else
         --  decreasing
         if Target_Reference_Count.all = 1 then
            Target_Max_Length.all := New_Length;
         end if;
      end if;
   end Set_Length;

end System.Reference_Counting;
