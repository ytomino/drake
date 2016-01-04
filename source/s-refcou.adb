package body System.Reference_Counting is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   procedure sync_add_and_fetch_32 (
      A1 : not null access Counter;
      A2 : Counter)
      with Import,
         Convention => Intrinsic, External_Name => "__sync_add_and_fetch_4";

   function sync_sub_and_fetch_32 (
      A1 : not null access Counter;
      A2 : Counter)
      return Counter
      with Import,
         Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_4";

   function sync_bool_compare_and_swap (
      A1 : not null access Length_Type;
      A2 : Length_Type;
      A3 : Length_Type)
      return Boolean
      with Convention => Intrinsic;
   pragma Inline_Always (sync_bool_compare_and_swap);

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
               return Boolean
               with Import,
                  Convention => Intrinsic,
                  External_Name => "__sync_bool_compare_and_swap_4";
         begin
            return sync_bool_compare_and_swap_32 (A1, A2, A3);
         end;
      else
         declare
            function sync_bool_compare_and_swap_64 (
               A1 : not null access Storage_Elements.Storage_Count;
               A2 : Storage_Elements.Storage_Offset;
               A3 : Storage_Elements.Storage_Offset)
               return Boolean
               with Import,
                  Convention => Intrinsic,
                  External_Name => "__sync_bool_compare_and_swap_8";
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
      New_Length : Length_Type;
      New_Capacity : Length_Type;
      Sentinel : not null Data_Access;
      Reallocate : not null access procedure (
         Target : aliased in out not null Data_Access;
         Length : Length_Type;
         Max_Length : Length_Type;
         Capacity : Length_Type);
      Copy : not null access procedure (
         Target : out not null Data_Access;
         Source : not null Data_Access;
         Length : Length_Type;
         Max_Length : Length_Type;
         Capacity : Length_Type);
      Free : not null access procedure (Object : in out Data_Access)) is
   begin
      if Shared (Target.all) then
         if New_Capacity /= Target_Capacity or else Target.all /= Sentinel then
            declare
               Old : aliased Container := Target.all;
            begin
               if New_Capacity = 0 then
                  Target.all := Sentinel;
               else
                  Copy (
                     Target.all,
                     Old,
                     Target_Length,
                     New_Length,
                     New_Capacity);
               end if;
               Clear (Old'Access, Free => Free);
            end;
         end if;
      else -- not shared
         if New_Capacity /= Target_Capacity then
            if New_Capacity = 0 then
               Free (Target.all);
               Target.all := Sentinel;
            else
               Reallocate (
                  Target.all,
                  Target_Length,
                  New_Length,
                  New_Capacity);
            end if;
         end if;
      end if;
   end Unique;

   procedure In_Place_Set_Length (
      Target_Data : not null Data_Access;
      Target_Length : Length_Type;
      Target_Max_Length : aliased in out Length_Type;
      Target_Capacity : Length_Type;
      New_Length : Length_Type;
      Failure : out Boolean) is
   begin
      if New_Length > Target_Length then
         --  inscreasing
         if New_Length > Target_Capacity then
            --  expanding
            Failure := True; -- should be reallocated
         else
            --  try to use reserved area
            if sync_bool_compare_and_swap (
               Target_Max_Length'Access,
               Target_Length,
               New_Length)
            then
               Failure := False; -- success
            elsif Shared (Target_Data) then
               Failure := True; -- should be copied
            else -- reference count = 1
               Target_Max_Length := New_Length;
               Failure := False; -- success
            end if;
         end if;
      else
         --  decreasing
         if not Shared (Target_Data) then
            Target_Max_Length := New_Length;
         end if;
         Failure := False; -- success
      end if;
   end In_Place_Set_Length;

end System.Reference_Counting;
