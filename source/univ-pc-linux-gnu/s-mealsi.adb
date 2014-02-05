with C.malloc;
function System.Memory.Allocated_Size (P : Address)
   return Storage_Elements.Storage_Count
is
   pragma Suppress (All_Checks);
begin
   return Storage_Elements.Storage_Count (
      C.malloc.malloc_usable_size (C.void_ptr (P)));
end System.Memory.Allocated_Size;
