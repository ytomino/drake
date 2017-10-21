with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
package body System.Storage_Pools.Unbounded is
   use type Storage_Elements.Storage_Offset;

   package HA_Conv is
      new Address_To_Named_Access_Conversions (Header, Header_Access);

   --  implementation

   overriding procedure Finalize (Object : in out Unbounded_Pool) is
   begin
      while Object.List /= null loop
         declare
            Next : constant Header_Access := Object.List.Next;
         begin
            Standard_Allocators.Free (HA_Conv.To_Address (Object.List));
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
      X : Address;
   begin
      X := Standard_Allocators.Allocate (
         Header'Size / Standard'Storage_Unit + Size_In_Storage_Elements);
      Storage_Address := X
         + Storage_Elements.Storage_Offset'(
            Header'Size / Standard'Storage_Unit);
      if Storage_Address mod Alignment /= 0 then
         Standard_Allocators.Free (X);
         raise Storage_Error;
      end if;
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
      pragma Unreferenced (Alignment);
      X : Address;
   begin
      X := Storage_Address
         - Storage_Elements.Storage_Offset'(
            Header'Size / Standard'Storage_Unit);
      if HA_Conv.To_Pointer (X).Previous = null then
         Pool.List := HA_Conv.To_Pointer (X).Next;
      else
         HA_Conv.To_Pointer (X).Previous.Next := HA_Conv.To_Pointer (X).Next;
      end if;
      HA_Conv.To_Pointer (X).Next.Previous := HA_Conv.To_Pointer (X).Previous;
      Standard_Allocators.Free (X);
   end Deallocate;

end System.Storage_Pools.Unbounded;
