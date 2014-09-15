pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Storage_Elements;
with System.Storage_Pools;
package System.Pool_Size is
   pragma Preelaborate;

   use type Storage_Elements.Storage_Offset;

   type Aligned_Storage_Array is new Storage_Elements.Storage_Array;
   for Aligned_Storage_Array'Alignment use Standard'Maximum_Alignment;

   --  mixed

   type Bounded_Allocator (
      Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is limited
   record
      First_Free : Storage_Elements.Storage_Offset := -1; -- offset
      First_Empty : Storage_Elements.Storage_Count := 0; -- offset
      Storage : aliased Aligned_Storage_Array (1 .. Size);
   end record;
   for Bounded_Allocator'Alignment use Standard'Maximum_Alignment;

   procedure Allocate (
      Allocator : aliased in out Bounded_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   procedure Deallocate (
      Allocator : aliased in out Bounded_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   function Storage_Size (Allocator : Bounded_Allocator)
      return Storage_Elements.Storage_Count;
   pragma Inline (Storage_Size);

   pragma Simple_Storage_Pool_Type (Bounded_Allocator);

   --  fixed

   type Bounded_Fixed_Allocator (
      Size : Storage_Elements.Storage_Count;
      Component_Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is limited
   record
      First_Free : Storage_Elements.Storage_Offset := -1; -- offset
      First_Empty : Storage_Elements.Storage_Count := 0; -- offset
      Storage : aliased Aligned_Storage_Array (1 .. Size);
   end record;
   for Bounded_Fixed_Allocator'Alignment use Standard'Maximum_Alignment;

   procedure Allocate (
      Allocator : aliased in out Bounded_Fixed_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   procedure Deallocate (
      Allocator : aliased in out Bounded_Fixed_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   function Storage_Size (Allocator : Bounded_Fixed_Allocator)
      return Storage_Elements.Storage_Count;
   pragma Inline (Storage_Size);

   pragma Simple_Storage_Pool_Type (Bounded_Fixed_Allocator);

   --  required for access types having explicit 'Storage_Size > 0 by compiler
   --  (s-poosiz.ads)
   type Stack_Bounded_Pool (
      Pool_Size : Storage_Elements.Storage_Count;
      Elmt_Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
      new Storage_Pools.Root_Storage_Pool with
   record
      case Elmt_Size is
         when 0 =>
            Mixed : aliased Bounded_Allocator (Pool_Size, Alignment);
         when others =>
            Fixed : aliased
               Bounded_Fixed_Allocator (Pool_Size, Elmt_Size, Alignment);
      end case;
   end record;
   pragma Finalize_Storage_Only (Stack_Bounded_Pool);

   overriding procedure Initialize (Pool : in out Stack_Bounded_Pool) is null;

   overriding procedure Allocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);
   pragma Inline (Allocate);

   overriding procedure Deallocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);
   pragma Inline (Deallocate);

   overriding function Storage_Size (Pool : Stack_Bounded_Pool)
      return Storage_Elements.Storage_Count;
   pragma Inline (Storage_Size);

end System.Pool_Size;
