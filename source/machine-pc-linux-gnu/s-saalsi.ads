pragma License (Unrestricted);
--  implementation unit specialized for Linux
function System.System_Allocators.Allocated_Size (
   Storage_Address : Address)
   return Storage_Elements.Storage_Count;
pragma Preelaborate (System.System_Allocators.Allocated_Size);
pragma Inline (System.System_Allocators.Allocated_Size);
