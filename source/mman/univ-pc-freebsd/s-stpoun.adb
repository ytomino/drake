with System.Memory;
with System.Address_To_Named_Access_Conversions;
package body System.Storage_Pools.Unbounded is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   package Conv is
      new Address_To_Named_Access_Conversions (Header, Header_Access);

   --  implementation

   overriding procedure Finalize (Object : in out Unbounded_Pool) is
   begin
      while Object.List /= null loop
         declare
            Next : constant Header_Access := Object.List.Next;
         begin
            Memory.Free (Conv.To_Address (Object.List));
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
      X := Memory.Allocate (
         Header'Size / Storage_Unit
         + Size_In_Storage_Elements);
      Storage_Address := X
         + Storage_Elements.Storage_Offset'(Header'Size / Storage_Unit);
      if Storage_Address mod Alignment /= 0 then
         Memory.Free (X);
         raise Storage_Error;
      end if;
      Conv.To_Pointer (X).Previous := Pool.List;
      Conv.To_Pointer (X).Next := null;
      Pool.List := Conv.To_Pointer (X);
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
         - Storage_Elements.Storage_Offset'(Header'Size / Storage_Unit);
      if Conv.To_Pointer (X).Previous = null then
         Pool.List := Conv.To_Pointer (X).Next;
      else
         Conv.To_Pointer (X).Previous.Next := Conv.To_Pointer (X).Next;
      end if;
      Conv.To_Pointer (X).Next.Previous := Conv.To_Pointer (X).Previous;
      Memory.Free (X);
   end Deallocate;

   overriding function Storage_Size (Pool : Unbounded_Pool)
      return Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return Storage_Elements.Storage_Count'Last;
   end Storage_Size;

end System.Storage_Pools.Unbounded;
