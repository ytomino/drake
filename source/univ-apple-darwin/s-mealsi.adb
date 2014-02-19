with C.malloc.malloc;
function System.Memory.Allocated_Size (
   Storage_Address : Address)
   return Storage_Elements.Storage_Count
is
   pragma Suppress (All_Checks);
begin
   return Storage_Elements.Storage_Count (
      C.malloc.malloc.malloc_size (C.void_const_ptr (Storage_Address)));
end System.Memory.Allocated_Size;
