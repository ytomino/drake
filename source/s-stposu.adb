pragma Check_Policy (Validate, Off);
with Ada.Exceptions;
with System.Address_To_Named_Access_Conversions;
with System.Runtime_Context;
with System.Shared_Locking;
package body System.Storage_Pools.Subpools is
   pragma Suppress (All_Checks);
   use type Finalization_Masters.Finalization_Master_Ptr;
   use type Finalization_Masters.Finalize_Address_Ptr;

   function sync_bool_compare_and_swap (
      A1 : not null access Finalization_State;
      A2 : Finalization_State;
      A3 : Finalization_State)
      return Boolean;
   pragma Import (Intrinsic, sync_bool_compare_and_swap,
      "__sync_bool_compare_and_swap_1");

   package FM_Node_Ptr_Conv is
      new Address_To_Named_Access_Conversions (
         Finalization_Masters.FM_Node,
         Finalization_Masters.FM_Node_Ptr);

   --  hooks for smart linking, making code of subpool as removable

   procedure Setup_Allocation (
      Pool : in out Root_Storage_Pool'Class;
      Context_Subpool : Subpool_Handle;
      Context_Master : Finalization_Masters.Finalization_Master_Ptr;
      Fin_Address : Finalization_Masters.Finalize_Address_Ptr;
      Subpool : out Subpool_Handle;
      Master : out Finalization_Masters.Finalization_Master_Ptr);
   procedure Setup_Allocation (
      Pool : in out Root_Storage_Pool'Class;
      Context_Subpool : Subpool_Handle;
      Context_Master : Finalization_Masters.Finalization_Master_Ptr;
      Fin_Address : Finalization_Masters.Finalize_Address_Ptr;
      Subpool : out Subpool_Handle;
      Master : out Finalization_Masters.Finalization_Master_Ptr)
   is
      pragma Unreferenced (Pool);
      pragma Unreferenced (Fin_Address);
      pragma Check (Validate, Context_Subpool = null);
   begin
      Subpool := null;
      Master := Context_Master;
   end Setup_Allocation;

   procedure Setup_Allocation_With_Subpools (
      Pool : in out Root_Storage_Pool'Class;
      Context_Subpool : Subpool_Handle;
      Context_Master : Finalization_Masters.Finalization_Master_Ptr;
      Fin_Address : Finalization_Masters.Finalize_Address_Ptr;
      Subpool : out Subpool_Handle;
      Master : out Finalization_Masters.Finalization_Master_Ptr);
   procedure Setup_Allocation_With_Subpools (
      Pool : in out Root_Storage_Pool'Class;
      Context_Subpool : Subpool_Handle;
      Context_Master : Finalization_Masters.Finalization_Master_Ptr;
      Fin_Address : Finalization_Masters.Finalize_Address_Ptr;
      Subpool : out Subpool_Handle;
      Master : out Finalization_Masters.Finalization_Master_Ptr) is
   begin
      if Pool in Root_Storage_Pool_With_Subpools'Class then
         if Context_Subpool = null then
            Subpool := Default_Subpool_For_Pool (
               Root_Storage_Pool_With_Subpools'Class (Pool));
         else
            Subpool := Context_Subpool;
         end if;
         if Subpool.Owner /=
            Root_Storage_Pool_With_Subpools'Class (Pool)'Unchecked_Access
         then
            raise Program_Error;
         end if;
         if Fin_Address /= null then
            Master := Subpool.Master'Access;
         end if;
      else
         Setup_Allocation (
            Pool,
            Context_Subpool,
            Context_Master,
            Fin_Address,
            Subpool,
            Master);
      end if;
   end Setup_Allocation_With_Subpools;

   Setup_Allocation_Hook : not null access
      procedure (
         Pool : in out Root_Storage_Pool'Class;
         Context_Subpool : Subpool_Handle;
         Context_Master : Finalization_Masters.Finalization_Master_Ptr;
         Fin_Address : Finalization_Masters.Finalize_Address_Ptr;
         Subpool : out Subpool_Handle;
         Master : out Finalization_Masters.Finalization_Master_Ptr) :=
      Setup_Allocation'Access;

   --  subpools and theirs owner

   procedure Attach (
      Owner : not null Root_Storage_Pool_With_Subpools_Access;
      Item : not null Subpool_Handle);

   procedure Detach (Item : not null Subpool_Handle);

   procedure Finalize_Subpool (Subpool : not null Subpool_Handle);

   procedure Attach (
      Owner : not null Root_Storage_Pool_With_Subpools_Access;
      Item : not null Subpool_Handle) is
   begin
      pragma Check (Validate, Item.Owner = null);
      Shared_Locking.Enter;
      Item.Owner := Owner;
      Item.Previous := Owner.Last;
      if Item.Previous /= null then
         Item.Previous.Next := Item;
      end if;
      Item.Next := null;
      Owner.Last := Item;
      Shared_Locking.Leave;
   end Attach;

   procedure Detach (Item : not null Subpool_Handle) is
   begin
      pragma Check (Validate, Item.Owner /= null);
      Shared_Locking.Enter;
      if Item.Previous /= null then
         Item.Previous.Next := Item.Next;
      end if;
      if Item.Next /= null then
         Item.Next.Previous := Item.Previous;
      else
         Item.Owner.Last := Item.Previous;
      end if;
      Item.Owner := null;
      Shared_Locking.Leave;
   end Detach;

   procedure Finalize_Subpool (Subpool : not null Subpool_Handle) is
   begin
      if Subpool.Owner /= null then
         Finalization_Masters.Finalize (Subpool.Master);
         Detach (Subpool);
      end if;
   end Finalize_Subpool;

   --  implementation

   function Pool_Of_Subpool (
      Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_With_Subpools'Class is
   begin
      return Subpool.Owner;
   end Pool_Of_Subpool;

   procedure Set_Pool_Of_Subpool (
      Subpool : not null Subpool_Handle;
      To : in out Root_Storage_Pool_With_Subpools'Class) is
   begin
      if Subpool.Owner /= null
         or else To.Finalization_State = FS_Finalization_Started
      then
         raise Program_Error;
      else
         Attach (To'Unrestricted_Access, Subpool);
      end if;
   end Set_Pool_Of_Subpool;

   function Default_Subpool_For_Pool (
      Pool : Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle is
   begin
      --  RM 13.11.4 (35/3)
      --  The pool implementor should override Default_Subpool_For_Pool
      --  if the pool is to support a default subpool for the pool.
      raise Program_Error;
      return Default_Subpool_For_Pool (Pool);
   end Default_Subpool_For_Pool;

   overriding procedure Allocate (
      Pool : in out Root_Storage_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Allocate_From_Subpool (
         Root_Storage_Pool_With_Subpools'Class (Pool),
         Storage_Address,
         Size_In_Storage_Elements,
         Alignment,
         Default_Subpool_For_Pool (
            Root_Storage_Pool_With_Subpools'Class (Pool)));
   end Allocate;

   procedure Unchecked_Deallocate_Subpool (Subpool : in out Subpool_Handle) is
   begin
      --  (s-spsufi.adb)
      if Subpool /= null then
         if Subpool.Owner = null then
            raise Program_Error;
         else
            declare
               Pool : constant access Root_Storage_Pool_With_Subpools'Class :=
                  Pool_Of_Subpool (Subpool); -- save it before finalize
            begin
               Finalize_Subpool (Subpool);
               Deallocate_Subpool (Pool.all, Subpool);
            end;
            Subpool := null;
         end if;
      end if;
   end Unchecked_Deallocate_Subpool;

   overriding procedure Initialize (
      Object : in out Root_Storage_Pool_With_Subpools)
   is
      pragma Unreferenced (Object);
   begin
      Setup_Allocation_Hook := Setup_Allocation_With_Subpools'Access;
   end Initialize;

   overriding procedure Finalize (
      Object : in out Root_Storage_Pool_With_Subpools) is
   begin
      if sync_bool_compare_and_swap (
         Object.Finalization_State'Access,
         FS_Active,
         FS_Finalization_Started)
      then
         declare
            X : Ada.Exceptions.Exception_Occurrence;
            Raised : Boolean := False;
         begin
            while Object.Last /= null loop
               begin
                  Finalize_Subpool (Object.Last);
               exception
                  when E : others =>
                     if not Raised then
                        Raised := True;
                        Ada.Exceptions.Save_Occurrence (X, E);
                     end if;
               end;
            end loop;
            if Raised then
               Ada.Exceptions.Unchecked_Reraise_Occurrence (X);
            end if;
         end;
      end if;
   end Finalize;

   procedure Allocate_Any_Controlled (
      Pool : in out Root_Storage_Pool'Class;
      Context_Subpool : Subpool_Handle;
      Context_Master : Finalization_Masters.Finalization_Master_Ptr;
      Fin_Address : Finalization_Masters.Finalize_Address_Ptr;
      Addr : out Address;
      Storage_Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Is_Controlled : Boolean;
      On_Subpool : Boolean)
   is
      Overlaid_Allocation : Address := Null_Address;
      Subpool : Subpool_Handle := null;
      Master : Finalization_Masters.Finalization_Master_Ptr := null;
      Actual_Storage_Address : Address;
      Actual_Size : Storage_Elements.Storage_Count;
      Header_And_Padding : Storage_Elements.Storage_Offset;
   begin
      Setup_Allocation_Hook (
         Pool,
         Context_Subpool,
         Context_Master,
         Fin_Address,
         Subpool,
         Master);
      pragma Check (Validate, On_Subpool <= (Subpool /= null));
      if Is_Controlled then
         if Master = null
            or else Finalization_Masters.Finalization_Started (Master.all)
            or else Fin_Address = null
         then
            raise Program_Error;
         else
            Header_And_Padding := Header_Size_With_Padding (Alignment);
            Actual_Size := Storage_Size + Header_And_Padding;
            declare
               TLS : constant
                  not null Runtime_Context.Task_Local_Storage_Access :=
                  Runtime_Context.Get_Task_Local_Storage;
            begin
               Overlaid_Allocation := TLS.Overlaid_Allocation;
            end;
         end if;
      else
         Actual_Size := Storage_Size;
      end if;
      --  allocation
      if Subpool /= null then
         Allocate_From_Subpool (
            Root_Storage_Pool_With_Subpools'Class (Pool),
            Actual_Storage_Address,
            Actual_Size,
            Alignment,
            Subpool);
      else
         Allocate (Pool, Actual_Storage_Address, Actual_Size, Alignment);
      end if;
      --  fix address
      if Is_Controlled
         and then -- for System.Storage_Pools.Overlaps
            Actual_Storage_Address /= Overlaid_Allocation
      then
         Shared_Locking.Enter;
         declare
            N_Ptr : constant Finalization_Masters.FM_Node_Ptr :=
               FM_Node_Ptr_Conv.To_Pointer (
                  Actual_Storage_Address
                  + Header_And_Padding
                  - Finalization_Masters.Header_Offset);
         begin
            Finalization_Masters.Attach_Unprotected (
               N_Ptr,
               Finalization_Masters.Objects_Unprotected (
                  Master.all,
                  Fin_Address));
         end;
         Addr := Actual_Storage_Address + Header_And_Padding;
         Finalization_Masters.Set_Finalize_Address_Unprotected (
            Master.all,
            Fin_Address);
         Shared_Locking.Leave;
      else
         Addr := Actual_Storage_Address;
      end if;
   end Allocate_Any_Controlled;

   procedure Deallocate_Any_Controlled (
      Pool : in out Root_Storage_Pool'Class;
      Addr : Address;
      Storage_Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Is_Controlled : Boolean)
   is
      Actual_Storage_Address : Address;
      Actual_Size : Storage_Elements.Storage_Count;
   begin
      --  fix address
      if Is_Controlled
         and then -- for System.Storage_Pools.Overlaps
            Runtime_Context.Get_Task_Local_Storage.Overlaid_Allocation /= Addr
      then
         Shared_Locking.Enter;
         declare
            Header_And_Padding : constant Storage_Elements.Storage_Offset :=
               Header_Size_With_Padding (Alignment);
            N_Ptr : constant Finalization_Masters.FM_Node_Ptr :=
               FM_Node_Ptr_Conv.To_Pointer (
                  Addr - Finalization_Masters.Header_Offset);
         begin
            Finalization_Masters.Detach_Unprotected (N_Ptr);
            Actual_Storage_Address := Addr - Header_And_Padding;
            Actual_Size := Storage_Size + Header_And_Padding;
         end;
         Shared_Locking.Leave;
      else
         Actual_Storage_Address := Addr;
         Actual_Size := Storage_Size;
      end if;
      --  deallocation
      Deallocate (Pool, Actual_Storage_Address, Actual_Size, Alignment);
   end Deallocate_Any_Controlled;

   function Header_Size_With_Padding (
      Alignment : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count is
   begin
      return Finalization_Masters.Header_Size
         + (-Finalization_Masters.Header_Size) mod Alignment;
   end Header_Size_With_Padding;

end System.Storage_Pools.Subpools;
