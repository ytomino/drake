with C.malloc_np;
function System.Memory.Allocated_Size (P : Address)
   return Storage_Elements.Storage_Count
is
   pragma Suppress (All_Checks);
begin
   return Storage_Elements.Storage_Count (
      C.malloc_np.malloc_usable_size (C.void_const_ptr (P)));
end System.Memory.Allocated_Size;
