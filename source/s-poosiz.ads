pragma License (Unrestricted);
--  implementation package required by compiler
with System.Storage_Elements;
with System.Storage_Pools;
package System.Pool_Size is
   pragma Preelaborate;

   use type Storage_Elements.Storage_Offset;

   --  required for access types having explicit 'Storage_Size > 0 by compiler
   --  (s-poosiz.ads)
   type Stack_Bounded_Pool (
      Pool_Size : Storage_Elements.Storage_Count;
      Elmt_Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
      new Storage_Pools.Root_Storage_Pool with
   record
      First_Free : Storage_Elements.Storage_Count := 0;
      First_Empty : Storage_Elements.Storage_Count := 1;
      Aligned_Elmt_Size : Storage_Elements.Storage_Count :=
         Storage_Elements.Storage_Offset'Max (
            (Storage_Elements.Storage_Count'Size + Standard'Storage_Unit - 1) /
               Standard'Storage_Unit,
            (Elmt_Size + Alignment - 1) / Alignment * Alignment);
      The_Pool : Storage_Elements.Storage_Array (1 .. Pool_Size);
   end record;

   procedure Initialize (Pool : in out Stack_Bounded_Pool) is null;

   overriding procedure Allocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding procedure Deallocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding function Storage_Size (Pool : Stack_Bounded_Pool)
      return Storage_Elements.Storage_Count;

end System.Pool_Size;
