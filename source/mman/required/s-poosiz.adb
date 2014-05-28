with System.Shared_Locking;
package body System.Pool_Size is
   pragma Suppress (All_Checks);

   overriding procedure Allocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Assert (Pool.Alignment rem Alignment = 0);
      Error : Boolean := False;
   begin
      Shared_Locking.Enter;
      if Pool.Elmt_Size = 0 then
         --  variable size mode (allocation only...)
         if Pool.First_Empty <=
            Pool.Pool_Size - Size_In_Storage_Elements + 1
         then
            Storage_Address := Pool.The_Pool (Pool.First_Empty)'Address;
            Pool.First_Empty := Pool.First_Empty
               + (Size_In_Storage_Elements + Pool.Alignment - 1)
                  / Pool.Alignment
                  * Pool.Alignment;
         else
            Error := True;
         end if;
      else
         --  fixed size mode
         pragma Assert (Size_In_Storage_Elements = Pool.Elmt_Size);
         if Pool.First_Free /= 0 then
            Storage_Address := Pool.The_Pool (Pool.First_Free)'Address;
            declare
               Next : Storage_Elements.Storage_Count;
               for Next'Address use Storage_Address;
            begin
               Pool.First_Free := Next;
            end;
         elsif Pool.First_Empty <=
            Pool.Pool_Size - Pool.Aligned_Elmt_Size + 1
         then
            Storage_Address := Pool.The_Pool (Pool.First_Empty)'Address;
            Pool.First_Empty := Pool.First_Empty + Pool.Aligned_Elmt_Size;
         else
            Error := True;
         end if;
      end if;
      Shared_Locking.Leave;
      if Error then
         raise Storage_Error;
      end if;
   end Allocate;

   overriding procedure Deallocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Assert (Pool.Alignment rem Alignment = 0);
   begin
      Shared_Locking.Enter;
      if Pool.Elmt_Size = 0 then
         --  variable size mode
         null; -- deallocation is unimplemented...
      else
         --  fixed size mode
         pragma Assert (Size_In_Storage_Elements = Pool.Elmt_Size);
         declare
            Next : Storage_Elements.Storage_Count;
            for Next'Address use Storage_Address;
         begin
            Next := Pool.First_Free;
            Pool.First_Free := Storage_Address - Pool.The_Pool'Address + 1;
         end;
      end if;
      Shared_Locking.Leave;
   end Deallocate;

   overriding function Storage_Size (Pool : Stack_Bounded_Pool)
      return Storage_Elements.Storage_Count is
   begin
      return Pool.Pool_Size;
   end Storage_Size;

end System.Pool_Size;
