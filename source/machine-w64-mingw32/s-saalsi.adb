with C.winbase;
with C.windef;
function System.System_Allocators.Allocated_Size (
   Storage_Address : Address)
   return Storage_Elements.Storage_Count
is
   pragma Suppress (All_Checks);
begin
   return Storage_Elements.Storage_Count (
      C.winbase.HeapSize (
         C.winbase.GetProcessHeap,
         0,
         C.windef.LPCVOID (Storage_Address)));
end System.System_Allocators.Allocated_Size;
