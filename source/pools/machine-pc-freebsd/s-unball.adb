with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.System_Allocators;
package body System.Unbounded_Allocators is
   use type Storage_Elements.Integer_Address;
   use type Storage_Elements.Storage_Offset;

   package HA_Conv is
      new Address_To_Named_Access_Conversions (Header, Header_Access);

   Header_Offset : constant Storage_Elements.Storage_Count :=
      Storage_Elements.Storage_Offset (
         (Header'Size / Standard'Storage_Unit
            + Storage_Elements.Integer_Address'(
               Standard'Maximum_Alignment - 1))
         and not (Standard'Maximum_Alignment - 1));

   procedure Deallocate (X : not null Header_Access);
   procedure Deallocate (X : not null Header_Access) is
   begin
      pragma Assert ((HA_Conv.To_Address (X.Previous) and 1) = 0);
      X.Previous.Next := X.Next;
      X.Next.Previous :=
         HA_Conv.To_Pointer (
            (HA_Conv.To_Address (X.Next.Previous) and 1)
            or HA_Conv.To_Address (X.Previous));
      System_Allocators.Free (HA_Conv.To_Address (X));
   end Deallocate;

   --  implementation

   procedure Initialize (Object : in out Unbounded_Allocator) is
      X : Address;
   begin
      X := System_Allocators.Allocate (Header'Size / Standard'Storage_Unit);
      HA_Conv.To_Pointer (X).Previous := HA_Conv.To_Pointer (1 or X);
      HA_Conv.To_Pointer (X).Next := HA_Conv.To_Pointer (X);
      Object := Unbounded_Allocator (HA_Conv.To_Pointer (X));
   end Initialize;

   procedure Finalize (Object : in out Unbounded_Allocator) is
   begin
      while Object.Next /= Header_Access (Object) loop
         Deallocate (Object.Next);
      end loop;
      System_Allocators.Free (HA_Conv.To_Address (Header_Access (Object)));
   end Finalize;

   procedure Allocate (
      Allocator : Unbounded_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      X : Address;
   begin
      X := System_Allocators.Allocate (
         Header'Size / Standard'Storage_Unit + Size_In_Storage_Elements,
         Alignment =>
            Storage_Elements.Storage_Offset'Max (Header'Alignment, Alignment));
      if X = Null_Address then
         Standard_Allocators.Raise_Heap_Exhausted;
      end if;
      Storage_Address := X + Header_Offset;
      HA_Conv.To_Pointer (X).Previous := Header_Access (Allocator);
      HA_Conv.To_Pointer (X).Next := Allocator.Next;
      HA_Conv.To_Pointer (X).Previous.Next := HA_Conv.To_Pointer (X);
      HA_Conv.To_Pointer (X).Next.Previous :=
         HA_Conv.To_Pointer (
            (HA_Conv.To_Address (HA_Conv.To_Pointer (X).Next.Previous) and 1)
            or X);
   end Allocate;

   procedure Deallocate (
      Allocator : Unbounded_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Allocator);
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
      X : constant not null Header_Access :=
         HA_Conv.To_Pointer (Storage_Address - Header_Offset);
   begin
      Deallocate (X);
   end Deallocate;

   function Allocator_Of (Storage_Address : Address)
      return Unbounded_Allocator is
      X : constant not null Header_Access :=
         HA_Conv.To_Pointer (Storage_Address - Header_Offset);
      I : not null Header_Access := X.Next;
   begin
      pragma Assert ((HA_Conv.To_Address (X.Previous) and 1) = 0);
      loop
         pragma Assert (I /= X);
         if (HA_Conv.To_Address (I.Previous) and 1) /= 0 then
            return Unbounded_Allocator (I);
         end if;
         I := I.Next;
      end loop;
   end Allocator_Of;

end System.Unbounded_Allocators;
