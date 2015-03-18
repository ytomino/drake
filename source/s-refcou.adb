package body System.Reference_Counting is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;

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
      A1 : not null access Length_Type;
      A2 : Length_Type;
      A3 : Length_Type)
      return Boolean;
   function sync_bool_compare_and_swap (
      A1 : not null access Length_Type;
      A2 : Length_Type;
      A3 : Length_Type)
      return Boolean
   is
      pragma Compile_Time_Error (
         Storage_Elements.Storage_Offset'Size /= 32
         and then Storage_Elements.Storage_Offset'Size /= 64,
         "Storage_Elements.Storage_Offset'Size is neither 32 nor 64");
   begin
      if Storage_Elements.Storage_Offset'Size = 32 then
         declare
            function sync_bool_compare_and_swap_32 (
               A1 : not null access Storage_Elements.Storage_Count;
               A2 : Storage_Elements.Storage_Offset;
               A3 : Storage_Elements.Storage_Offset)
               return Boolean;
            pragma Import (Intrinsic, sync_bool_compare_and_swap_32,
               "__sync_bool_compare_and_swap_4");
         begin
            return sync_bool_compare_and_swap_32 (A1, A2, A3);
         end;
      else
         declare
            function sync_bool_compare_and_swap_64 (
               A1 : not null access Storage_Elements.Storage_Count;
               A2 : Storage_Elements.Storage_Offset;
               A3 : Storage_Elements.Storage_Offset)
               return Boolean;
            pragma Import (Intrinsic, sync_bool_compare_and_swap_64,
               "__sync_bool_compare_and_swap_8");
         begin
            return sync_bool_compare_and_swap_64 (A1, A2, A3);
         end;
      end if;
   end sync_bool_compare_and_swap;

   --  implementation

   function Shared (Data : not null Data_Access) return Boolean is
   begin
      return Data.all > 1; -- static is True
   end Shared;

   --  not null because using sentinel (that means empty data block)

   procedure Adjust (
      Target : not null access Container)
   is
      Reference_Count : constant not null Data_Access := Target.all;
   begin
      if Reference_Count.all /= Static then
         sync_add_and_fetch_32 (Reference_Count, 1);
      end if;
   end Adjust;

   procedure Assign (
      Target : not null access Container;
      Source : not null access constant Container;
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      if Target.all /= Source.all then
         Clear (Target, Free);
         Target.all := Source.all;
         Adjust (Target);
      end if;
   end Assign;

   procedure Clear (
      Target : not null access Container;
      Free : not null access procedure (Object : in out Data_Access))
   is
      Reference_Count : constant not null Data_Access := Target.all;
   begin
      if Reference_Count.all /= Static then
         if sync_sub_and_fetch_32 (Reference_Count, 1) = 0 then
            Free (Target.all);
         end if;
      end if;
   end Clear;

   procedure Move (
      Target : not null access Container;
      Source : not null access Container;
      Sentinel : not null Data_Access;
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      if Target.all /= Source.all then
         Clear (Target, Free);
         Target.all := Source.all;
         Source.all := Sentinel;
      end if;
   end Move;

   procedure Unique (
      Target : not null access Container;
      Target_Length : Length_Type;
      Target_Capacity : Length_Type;
      Max_Length : Length_Type;
      Capacity : Length_Type;
      Sentinel : not null Data_Access;
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Length_Type;
         Max_Length : Length_Type;
         Capacity : Length_Type);
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      if Capacity /= Target_Capacity
         or else -- static (excluding Sentinel) is True
            (Target.all /= Sentinel and then Target.all.all > 1)
      then
         declare
            Old : aliased Container := Target.all;
         begin
            if Capacity = 0 then
               Target.all := Sentinel;
            else
               Copy (Target.all, Old, Target_Length, Max_Length, Capacity);
            end if;
            Clear (Old'Access, Free => Free);
         end;
      end if;
   end Unique;

   procedure Set_Length (
      Target : not null access Container;
      Target_Length : Length_Type;
      Target_Max_Length : aliased in out Length_Type;
      Target_Capacity : Length_Type;
      New_Length : Length_Type;
      Sentinel : not null Data_Access;
      Copy : not null access procedure (
         Target : out Data_Access;
         Source : not null Data_Access;
         Length : Length_Type;
         Max_Length : Length_Type;
         Capacity : Length_Type);
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      if New_Length > Target_Length then
         --  inscreasing
         if New_Length > Target_Capacity then
            --  expanding
            declare
               New_Capacity : constant Length_Type :=
                  Storage_Elements.Storage_Offset'Max (
                     Target_Capacity * 2,
                     New_Length);
            begin
               Unique (
                  Target => Target,
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
               Target_Max_Length'Access,
               Target_Length,
               New_Length)
            then
               null;
            elsif Target.all.all > 1 then
               Unique (
                  Target => Target,
                  Target_Length => Target_Length,
                  Target_Capacity => Target_Capacity,
                  Max_Length => New_Length,
                  Capacity => Target_Capacity, -- keep Capacity
                  Sentinel => Sentinel,
                  Copy => Copy, -- Copy should set Max_Length
                  Free => Free);
            else -- reference count = 1
               Target_Max_Length := New_Length;
            end if;
         end if;
      else
         --  decreasing
         if Target.all.all = 1 then
            Target_Max_Length := New_Length;
         end if;
      end if;
   end Set_Length;

end System.Reference_Counting;
