with System.Shared_Locking;
package body System.Pool_Size is
   pragma Suppress (All_Checks);

   type Unaligned_Storage_Offset is new Storage_Elements.Storage_Offset;
   for Unaligned_Storage_Offset'Alignment use 1;

   --  implementation of mixed

   procedure Allocate (
      Allocator : aliased in out Bounded_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      Error : Boolean := False;
   begin
      if Allocator.Alignment rem Alignment /= 0 then
         raise Constraint_Error;
      end if;
      Shared_Locking.Enter;
      declare
         Storage_Index : Storage_Elements.Storage_Count;
         Next_Empty : Storage_Elements.Storage_Count;
      begin
         Storage_Index := Allocator.First_Empty
            + (-Allocator.First_Empty) mod Allocator.Alignment;
         Next_Empty := Storage_Index + Size_In_Storage_Elements;
         if Next_Empty <= Allocator.Size then
            Allocator.First_Empty := Next_Empty;
            Storage_Address := Allocator.Storage'Address + Storage_Index;
         else
            Error := True;
         end if;
      end;
      Shared_Locking.Leave;
      if Error then
         raise Storage_Error;
      end if;
   end Allocate;

   procedure Deallocate (
      Allocator : aliased in out Bounded_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      null; -- deallocation is unimplemented...
   end Deallocate;

   function Storage_Size (Allocator : Bounded_Allocator)
      return Storage_Elements.Storage_Count is
   begin
      return Allocator.Size;
   end Storage_Size;

   --  implementation of fixed

   procedure Allocate (
      Allocator : aliased in out Bounded_Fixed_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      Aligned_Component_Size : constant Storage_Elements.Storage_Count :=
         Storage_Elements.Storage_Offset'Max (
            --  minimum chunk size
            (Storage_Elements.Storage_Count'Size + Standard'Storage_Unit - 1) /
               Standard'Storage_Unit,
            --  component size
            Allocator.Component_Size
               + (-Allocator.Component_Size) mod Allocator.Alignment);
      Error : Boolean := False;
   begin
      if Allocator.Alignment rem Alignment /= 0
         or else Size_In_Storage_Elements /= Allocator.Component_Size
      then
         raise Constraint_Error;
      end if;
      Shared_Locking.Enter;
      if Allocator.First_Free >= 0 then
         Storage_Address := Allocator.Storage'Address + Allocator.First_Free;
         declare
            Next : Unaligned_Storage_Offset;
            for Next'Address use Storage_Address;
         begin
            Allocator.First_Free := Storage_Elements.Storage_Offset (Next);
         end;
      elsif Allocator.First_Empty <=
         Allocator.Size - Aligned_Component_Size + 1
      then
         Storage_Address := Allocator.Storage'Address + Allocator.First_Empty;
         Allocator.First_Empty :=
            Allocator.First_Empty + Aligned_Component_Size;
      else
         Error := True;
      end if;
      Shared_Locking.Leave;
      if Error then
         raise Storage_Error;
      end if;
   end Allocate;

   procedure Deallocate (
      Allocator : aliased in out Bounded_Fixed_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      if Size_In_Storage_Elements /= Allocator.Component_Size
         or else Alignment /= Allocator.Alignment
      then
         raise Constraint_Error;
      end if;
      Shared_Locking.Enter;
      declare
         Next : Unaligned_Storage_Offset;
         for Next'Address use Storage_Address;
      begin
         Next := Unaligned_Storage_Offset (Allocator.First_Free);
      end;
      Allocator.First_Free := Storage_Address - Allocator.Storage'Address;
      Shared_Locking.Leave;
   end Deallocate;

   function Storage_Size (Allocator : Bounded_Fixed_Allocator)
      return Storage_Elements.Storage_Count is
   begin
      return Allocator.Size;
   end Storage_Size;

   --  implementation of required by compiler

   overriding procedure Allocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      case Pool.Elmt_Size is
         when 0 =>
            Allocate (
               Pool.Mixed,
               Storage_Address,
               Size_In_Storage_Elements,
               Alignment);
         when others =>
            Allocate (
               Pool.Fixed,
               Storage_Address,
               Size_In_Storage_Elements,
               Alignment);
      end case;
   end Allocate;

   overriding procedure Deallocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      case Pool.Elmt_Size is
         when 0 =>
            Deallocate (
               Pool.Mixed,
               Storage_Address,
               Size_In_Storage_Elements,
               Alignment);
         when others =>
            Deallocate (
               Pool.Fixed,
               Storage_Address,
               Size_In_Storage_Elements,
               Alignment);
      end case;
   end Deallocate;

   overriding function Storage_Size (Pool : Stack_Bounded_Pool)
      return Storage_Elements.Storage_Count is
   begin
      case Pool.Elmt_Size is
         when 0 =>
            return Storage_Size (Pool.Mixed);
         when others =>
            return Storage_Size (Pool.Fixed);
      end case;
   end Storage_Size;

end System.Pool_Size;
