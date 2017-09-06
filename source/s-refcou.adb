with System.Storage_Barriers;
package body System.Reference_Counting is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   function atomic_load (
      ptr : not null access constant Counter;
      memorder : Integer := Storage_Barriers.ATOMIC_ACQUIRE)
      return Counter
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_4";

   procedure atomic_add_fetch (
      ptr : not null access Counter;
      val : Counter;
      memorder : Integer := Storage_Barriers.ATOMIC_ACQ_REL)
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_add_fetch_4";

   function atomic_sub_fetch (
      ptr : not null access Counter;
      val : Counter;
      memorder : Integer := Storage_Barriers.ATOMIC_ACQ_REL)
      return Counter
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_sub_fetch_4";

   pragma Compile_Time_Error (
      Storage_Elements.Storage_Offset'Size /= 32
         and then Storage_Elements.Storage_Offset'Size /= 64,
      "Storage_Elements.Storage_Offset'Size is neither 32 nor 64");

   --  Use sequentially consistent model because an object's length and
   --    contents should be synchronized.
   Order : constant := Storage_Barriers.ATOMIC_SEQ_CST;

   function atomic_compare_exchange (
      ptr : not null access Length_Type;
      expected : not null access Length_Type;
      desired : Length_Type;
      weak : Boolean := False;
      success_memorder : Integer := Order;
      failure_memorder : Integer := Order)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name =>
            (case Storage_Elements.Storage_Offset'Size is
               when 32 => "__atomic_compare_exchange_4",
               when others => "__atomic_compare_exchange_8");

   --  implementation

   function Shared (Data : not null Data_Access) return Boolean is
   begin
      return atomic_load (Data) > 1; -- static is True
   end Shared;

   --  not null because using sentinel (that means empty data block)

   procedure Adjust (
      Target : not null access Container)
   is
      Reference_Count : constant not null Data_Access := Target.all;
   begin
      if atomic_load (Reference_Count) /= Static then
         atomic_add_fetch (Reference_Count, 1);
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
      if atomic_load (Reference_Count) /= Static
         and then atomic_sub_fetch (Reference_Count, 1) = 0
      then
         Free (Target.all);
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
            declare
               Expected : aliased Length_Type := Target_Length;
            begin
               Failure := not atomic_compare_exchange (
                  Target_Max_Length'Access,
                  Expected'Access,
                  New_Length);
            end;
            if Failure and then not Shared (Target_Data) then
               --  reference count = 1
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
