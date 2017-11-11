with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.System_Allocators;
package body System.Storage_Pools.Unbounded is
   use type Storage_Elements.Integer_Address;
   use type Storage_Elements.Storage_Offset;

   function popcountl (x : Storage_Elements.Storage_Offset) return Integer
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_popcountl";

   package HA_Conv is
      new Address_To_Named_Access_Conversions (Header, Header_Access);

   function Header_Offset (Alignment : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count;
   function Header_Offset (Alignment : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count is
   begin
      return Storage_Elements.Storage_Offset (
         (Header'Size / Standard'Storage_Unit
            + Storage_Elements.Integer_Address (Alignment - 1))
         and not Storage_Elements.Integer_Address'Base (Alignment));
   end Header_Offset;

   --  implementation

   overriding procedure Finalize (Object : in out Unbounded_Pool) is
   begin
      while Object.List /= null loop
         declare
            Next : constant Header_Access := Object.List.Next;
         begin
            System_Allocators.Free (HA_Conv.To_Address (Object.List));
            Object.List := Next;
         end;
      end loop;
   end Finalize;

   overriding procedure Allocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Check (Pre,
         Check => popcountl (Alignment) = 1 or else raise Storage_Error);
      X : Address;
   begin
      X := System_Allocators.Allocate (
         Header'Size / Standard'Storage_Unit + Size_In_Storage_Elements,
         Alignment => Alignment);
      if X = Null_Address then
         Standard_Allocators.Raise_Heap_Exhausted;
      end if;
      Storage_Address := X + Header_Offset (Alignment);
      HA_Conv.To_Pointer (X).Previous := Pool.List;
      HA_Conv.To_Pointer (X).Next := null;
      Pool.List := HA_Conv.To_Pointer (X);
   end Allocate;

   overriding procedure Deallocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Check (Pre,
         Check => popcountl (Alignment) = 1 or else raise Storage_Error);
      X : Address;
   begin
      X := Storage_Address - Header_Offset (Alignment);
      if HA_Conv.To_Pointer (X).Previous = null then
         Pool.List := HA_Conv.To_Pointer (X).Next;
      else
         HA_Conv.To_Pointer (X).Previous.Next := HA_Conv.To_Pointer (X).Next;
      end if;
      HA_Conv.To_Pointer (X).Next.Previous := HA_Conv.To_Pointer (X).Previous;
      System_Allocators.Free (X);
   end Deallocate;

end System.Storage_Pools.Unbounded;
